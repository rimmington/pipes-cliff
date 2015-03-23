{-# LANGUAGE FlexibleContexts #-}

-- | This contains the innards of Cliff.  You probably won't need
-- anything that's in here; "Pipes.Cliff" re-exports the most useful
-- bindings.  But nothing will break if you use what's in here, so
-- it's here if you need it.
module Pipes.Cliff.Core where

import System.Environment
import Data.List (intersperse)
import Control.Exception (IOException)
import System.IO
import qualified System.Process as Process
import System.Process (ProcessHandle)
import Pipes
import Pipes.Safe
import qualified Data.ByteString as BS
import qualified Pipes.Concurrent as PC
import Data.ByteString (ByteString)
import Control.Concurrent.MVar
import Control.Concurrent.Async
import Control.Monad.Trans.Reader
import System.Exit

-- * Data types

-- | Like 'Process.CmdSpec' in "System.Process", but also has an
-- instance for 'Show'.
data CmdSpec
  = ShellCommand String
  | RawCommand FilePath [String]
  deriving (Eq, Ord, Show)

convertCmdSpec :: CmdSpec -> Process.CmdSpec
convertCmdSpec (ShellCommand s) = Process.ShellCommand s
convertCmdSpec (RawCommand p ss) = Process.RawCommand p ss

-- ** Errors

-- | When dealing with a 'Handle', errors can occur when reading from,
-- writing to, or closing the handle.
data Activity
  = Reading
  | Writing
  | Closing
  deriving (Eq, Ord, Show)

-- | Describes a handle.  From the perspective of the subprocess; for
-- example, 'Input' means that this handle is connected to the
-- process's standard input.
data HandleDesc
  = Input
  | Output
  | Error
  deriving (Eq, Ord, Show)

-- | Describes IO errors tha occur when dealing with a 'Handle'.
data HandleOopsie = HandleOopsie Activity HandleDesc
  deriving (Eq,Show)

-- | Describes all IO exceptions.  The 'Oopsie' contains the
-- 'IOException' itself, along with the 'CmdSpec' that was running
-- when the exception occurred.  If the exception occurred while
-- dealing with a 'Handle', there is also a 'HandleOopsie'.  If there
-- is no 'HandleOopsie', this means that the exception arose when
-- running 'terminateProcess'.
--
-- The exceptions that are caught and placed into an 'Oopsie' may
-- arise from reading data from or writing data to a 'Handle'.  In
-- these errors, the associated 'Producer' or 'Consumer' will
-- terminate (which may trigger various cleanup actions in the
-- 'MonadSafe' computation) but the exception itself is not re-thrown;
-- rather, it is passed to the 'handler'.  Similarly, an exception may
-- occur while closing a handle; these exceptions are caught, not
-- rethrown, and are passed to the 'handler'.  If an exception arises
-- when terminating a process (I'm not sure this is possible) then it
-- is also caught, not rethrown, and passed to the 'handler'.
--
-- If an exception arises when creating a process--such as a command
-- not being found--the exception is /not/ caught, handled, or passed
-- to the 'handler'.  Also, an 'Oopsie' is created only for an
-- 'IOException'; no other exceptions of any kind are caught or
-- handled.  However, exceptions of any kind will still trigger
-- appropriate cleanup actions in the 'MonadSafe' computation.
data Oopsie = Oopsie (Maybe HandleOopsie) CmdSpec IOException
  deriving (Eq, Show)

-- | Formats an 'Oopsie' for display.
renderOopsie
  :: String
  -- ^ The name of the currently runnning program
  -> Oopsie
  -> String
renderOopsie pn (Oopsie mayHan cmd ioe) =
  pn ++ ": warning: when running command "
  ++ renderCommand cmd ++ ": " ++ renderMayHan mayHan
  ++ ": " ++ show ioe
  where
    renderCommand (ShellCommand str) = show str
    renderCommand (RawCommand fp ss)
      = concat . intersperse " " . map show
      $ fp : ss

    renderMayHan Nothing = "when terminating process"
    renderMayHan (Just (HandleOopsie act desc)) =
      "when " ++ actStr ++ " " ++ descStr
      where
        actStr = case act of
          Reading -> "reading from"
          Writing -> "writing to"
          Closing -> "closing the handle associated with"
        descStr = "standard " ++ case desc of
          Input -> "input"
          Output -> "output"
          Error -> "error"

-- | The default handler when receiving an 'Oopsie'; simply uses
-- 'renderOopsie' to format it nicely and put it on standard error.
defaultHandler :: Oopsie -> IO ()
defaultHandler oops = do
  pn <- getProgName
  hPutStrLn stderr $ renderOopsie pn oops

-- ** Configuration types

-- | How will the subprocess get its information for this stream?  A
-- 'NonPipe' is used for streams that will not be assigned to a
-- 'Proxy' but, instead, will be inherited from the parent or directed
-- from an existing 'Handle'.
data NonPipe
  = Inherit
  -- ^ Use whatever stream that the parent process has.
  | UseHandle Handle
  -- ^ Use the given handle for input or output

convertNonPipe :: Maybe NonPipe -> Process.StdStream
convertNonPipe a = case a of
  Nothing -> Process.CreatePipe
  Just Inherit -> Process.Inherit
  Just (UseHandle h) -> Process.UseHandle h

-- | Like 'System.Process.CreateProcess' in "System.Process",
-- this gives the necessary information to create a subprocess.  All
-- but one of these fields is also present in
-- 'System.Process.CreateProcess', and they all have the same meaning;
-- the only field that is different is the 'quiet' field.
data CreateProcess = CreateProcess
  { cmdspec :: CmdSpec
    -- ^ Executable and arguments, or shell command

  , cwd :: Maybe FilePath
  -- ^ A new current working directory for the subprocess; if
  -- 'Nothing', use the calling process's working directory.

  , env :: Maybe [(String, String)]
  -- ^ The environment for the subprocess; if 'Nothing', use the
  -- calling process's working directory.

  , close_fds :: Bool
  -- ^ If 'True', close all file descriptors other than the standard
  -- descriptors.  See the documentation for
  -- 'System.Process.close_fds' for details on how this works in
  -- Windows.

  , create_group :: Bool
  -- ^ If 'True', create a new process group.

  , delegate_ctlc :: Bool
  -- ^ See 'System.Process.delegate_ctlc' in the "System.Process"
  -- module for details.

  , storeProcessHandle :: Maybe (MVar Process.ProcessHandle)
  -- ^ To get the 'ProcessHandle' that results after starting the
  -- process, put a @Just@ here, with the 'MVar' in which you would
  -- like the 'ProcessHandle' to be stored.  The various functions
  -- that create a subprocess will store the process handle here
  -- shortly after the process is created.  You can then use
  -- 'waitForProcess' on the 'ProcessHandle', or you might want to use
  -- 'terminateProcess' on it.
  --
  -- Be sure you don't use 'takeMVar' until you have run the 'Effect'
  -- that creates the relevant process; otherwise your program will
  -- deadlock.
  --
  -- If you don't care about the process handle, just leave this set
  -- at 'Nothing'.

  , handler :: Oopsie -> IO ()
  -- ^ Whenever an IO exception arises during the course of various IO
  -- actios, the exception is caught and placed into an 'Oopsie' that
  -- indicates why and where the exception happened.  The 'handler'
  -- determines what happens when an 'Oopsie' comes in.  See 'Oopsie'
  -- for details.
  --
  -- The default 'handler' created by 'procSpec' is 'defaultHandler',
  -- which will simply print the exceptions to standard error.  You
  -- may not want to see the exceptions at all.  For example, many
  -- exceptions come from broken pipes.  A broken pipe might be
  -- entirely normal in your circumstance.  For example, if you are
  -- streaming a large set of values to a pager such as @less@ and you
  -- expect that the user will often quit the pager without viewing
  -- the whole result, a broken pipe will result, which will print a
  -- warning message.  That can be a nuisance.
  --
  -- If you don't want to see the exceptions at all, just set
  -- 'handler' to 'squelch', which simply discards the exceptions.
  --
  -- Conceivably you could rig up an elaborate mechanism that puts the
  -- 'Oopsie's into a "Pipes.Concurrent" mailbox or something.
  }

-- | Do not show or do anything with exceptions; useful to use as a
-- 'handler'.
squelch :: Monad m => a -> m ()
squelch = const (return ())

-- | Create a 'CreateProcess' record with default settings.  The
-- default settings are:
--
-- * a raw command (as opposed to a shell command) is created
--
-- * the current working directory is not changed from the parent process
--
-- * the environment is not changed from the parent process
--
-- * the parent's file descriptors (other than standard input,
-- standard output, and standard error) are inherited
--
-- * no new process group is created
--
-- * 'delegate_ctlc' is 'False'
--
-- * 'storeProcessHandle' is 'Nothing'
--
-- * 'handler' is 'defaultHandler'

procSpec
  :: String
  -- ^ The name of the program to run, such as @less@.
  -> [String]
  -- ^ Command-line arguments
  -> CreateProcess
procSpec prog args = CreateProcess
  { cmdspec = RawCommand prog args
  , cwd = Nothing
  , env = Nothing
  , close_fds = False
  , create_group = False
  , delegate_ctlc = False
  , storeProcessHandle = Nothing
  , handler = defaultHandler
  }

convertCreateProcess
  :: Maybe NonPipe
  -> Maybe NonPipe
  -> Maybe NonPipe
  -> CreateProcess
  -> Process.CreateProcess
convertCreateProcess inp out err a = Process.CreateProcess
  { Process.cmdspec = convertCmdSpec $ cmdspec a
  , Process.cwd = cwd a
  , Process.env = env a
  , Process.std_in = conv inp
  , Process.std_out = conv out
  , Process.std_err = conv err
  , Process.close_fds = close_fds a
  , Process.create_group = create_group a
  , Process.delegate_ctlc = delegate_ctlc a
  }
  where
    conv = convertNonPipe

-- * Environment

-- | A common environment for many functions.  Contains just the
-- necessary subset from 'CreateProcess'.
data Env = Env
  { envErrorHandler :: Oopsie -> IO ()
  , envProcessHandle :: Maybe (MVar ProcessHandle)
  , envCmdSpec :: CmdSpec
  }

envFromCreateProcess
  :: CreateProcess
  -> Env
envFromCreateProcess cp = Env
  { envErrorHandler = handler cp
  , envProcessHandle = storeProcessHandle cp
  , envCmdSpec = cmdspec cp
  }


-- * Exception handling

-- | Sends an exception using the exception handler specified in the
-- 'Env'.
handleException
  :: MonadIO m
  => Maybe HandleOopsie
  -> IOException
  -> ReaderT Env m ()
handleException mayOops exc = do
  spec <- asks envCmdSpec
  sender <- asks envErrorHandler
  let oops = Oopsie mayOops spec exc
  liftIO $ sender oops

-- | Run an action, taking all IO errors and sending them to the handler.
handleErrors
  :: (MonadCatch m, MonadIO m)
  => Maybe HandleOopsie
  -> m ()
  -> ReaderT Env m ()
handleErrors mayHandleOops act = catch act' catcher
  where
    act' = lift act
    catcher e = do
      spec <- asks envCmdSpec
      hndlr <- asks envErrorHandler
      let oops = Oopsie mayHandleOops spec e
      liftIO $ hndlr oops
      return ()

closeHandleNoThrow
  :: (MonadCatch m, MonadIO m)
  => Handle
  -> HandleDesc
  -> ReaderT Env m ()
closeHandleNoThrow hand desc = handleErrors (Just (HandleOopsie Closing desc))
  (liftIO $ hClose hand)

terminateProcess
  :: (MonadCatch m, MonadIO m)
  => Process.ProcessHandle
  -> ReaderT Env m ()
terminateProcess han = do
  _ <- handleErrors Nothing (liftIO (Process.terminateProcess han))
  handleErrors Nothing . liftIO $ do
    _ <- Process.waitForProcess han
    return ()


-- | Acquires a resource and ensures it will be destroyed when the
-- 'MonadSafe' computation completes.
acquire
  :: MonadSafe m
  => Base m a
  -- ^ Acquirer.
  -> (a -> Base m ())
  -- ^ Destroyer.
  -> m a
acquire acq rel = mask $ \restore -> do
  a <- liftBase acq
  _ <- register (rel a)
  restore $ return a


-- * Threads

-- | Runs a thread in the background.  The thread is terminated when
-- the 'MonadSafe' computation completes.
background
  :: MonadSafe m
  => IO a
  -> m (Async a)
background act = acquire (liftIO $ async act) (liftIO . cancel)

-- | Runs in the background an effect, typically one that is moving
-- data from one process to another.  For examples of its usage, see
-- "Pipes.Cliff.Examples".  The associated thread is killed when the
-- 'SafeT' computation completes.
conveyor :: Effect (SafeT IO) () -> SafeT IO ()
conveyor efct
  = (background . liftIO . runSafeT . runEffect $ efct) >> return ()

-- | A version of 'Control.Concurrent.Async.wait' with an overloaded
-- 'MonadIO' return type.  Allows you to wait for the return value of
-- threads launched with 'background'.  If the thread throws an
-- exception, 'waitForThread' will throw that same exception.
waitForThread :: MonadIO m => Async a -> m a
waitForThread = liftIO . wait

-- | An overloaded version of the 'Process.waitForProcess' from
-- "System.Process".
waitForProcess :: MonadIO m => ProcessHandle -> m ExitCode
waitForProcess h = liftIO $ Process.waitForProcess h

-- | Runs a single 'Effect' in the foreground.
runCliff :: (MonadMask m, MonadIO m) => Effect (SafeT m) a -> m a
runCliff = runSafeT . runEffect

-- * Mailboxes

-- | A buffer that holds 1 message.  I have no idea if this is the
-- ideal size.  Don't use an unbounded buffer, though, because with
-- unbounded producers an unbounded buffer will fill up your RAM.
--
-- Since the buffer just holds one size, you might think \"why not
-- just use an MVar\"?  At least, I have been silly enough to think
-- that.  Using @Pipes.Concurrent@ also give the mailbox the ability
-- to be sealed; sealing the mailbox signals to the other side that it
-- won't be getting any more input or be allowed to send any more
-- output, which tells the whole pipeline to start shutting down.
messageBuffer :: PC.Buffer a
messageBuffer = PC.bounded 1

-- | Creates a new mailbox for sending data to a process.  Returns a
-- 'PC.Output' which is the sink that accepts values for delivery to
-- the process, as well as a 'Producer' that will produce what comes
-- from the mailbox; the 'Producer' should be hooked to the standard
-- input of the subprocess.  If the Producer shuts down for any
-- reason, it also seals the mailbox, so the 'PC.Output' will not
-- accept any new values.
newMailboxToProcess
  :: (MonadSafe m, MonadSafe mo)
  => m (PC.Output a, Producer a mo ())
newMailboxToProcess = do
  let destroy (_, _, seal) = liftIO $ PC.atomically seal
  (sndr, PC.Input rcvr, seal) <-
    acquire (liftIO $ PC.spawn' messageBuffer) destroy
  let prod = do
        _ <- register (liftIO $ PC.atomically seal)
        go
      go = do
        mayVal <- liftIO $ PC.atomically rcvr
        maybe (return ()) (\v -> yield v >> go) mayVal
  return (sndr, prod)

-- | Creates a new mailbox for receiving data from a process.  Returns
-- a 'PC.Input' which is an exhaustible source of values from the
-- process, as well as s 'Consumer' that will consume values and place
-- them into the mailbox; this 'Consumer' should be hooked to the
-- desired stream (output or error) of the process.  If the Consumer
-- shuts down for any reason, it also seals the mailbox, so the
-- 'PC.Input' will not accept any new values.
newMailboxFromProcess
  :: (MonadSafe m, MonadSafe mi)
  => m (Consumer a mi (), PC.Input a)
newMailboxFromProcess = do
  let destroy (_, _, seal) = liftIO $ PC.atomically seal
  (PC.Output sndr, rcvr, seal) <-
    acquire (liftIO $ PC.spawn' messageBuffer) destroy
  let csmr = do
        _ <- register (liftIO $ PC.atomically seal)
        go
      go = do
        val <- await
        rslt <- liftIO $ PC.atomically (sndr val)
        if rslt then go else return ()
  return (csmr, rcvr)


-- * Production from and consumption to 'Handle's

-- | I have no idea what this should be.  I'll start with a simple
-- small value and see how it works.
bufSize :: Int
bufSize = 1024

-- | Create a 'Producer' that produces from a 'Handle'.  Takes
-- ownership of the 'Handle'; closes it when the 'Producer'
-- terminates.  If any IO errors arise either during production or
-- when the 'Handle' is closed, they are caught and passed to the
-- handler.
produceFromHandle
  :: MonadSafe m
  => HandleDesc
  -> Handle
  -> Reader Env (Producer ByteString m ())
produceFromHandle hDesc h = fmap f ask
  where
    f ev = catch produce hndlr
      where
        closeHan = runReaderT (closeHandleNoThrow h hDesc) ev
        produce = liftIO (BS.hGetSome h bufSize) >>= go
        go bs
            | BS.null bs = closeHan
            | otherwise = yield bs >> produce
        hndlr e = do
          closeHan
          lift (runReaderT (handleException oops e) ev)
    oops = Just (HandleOopsie Reading hDesc)


-- | Create a 'Consumer' that consumes from a 'Handle'.  Takes
-- ownership of the 'Handle'; closes it when the 'Consumer'
-- terminates.  If any IO errors arise either during consumption or
-- when the 'Handle' is closed, they are caught and passed to the
-- handler.
consumeToHandle
  :: MonadSafe m
  => Handle
  -> Reader Env (Consumer ByteString m ())
consumeToHandle h = fmap f ask
  where
    f ev = catch consume hndlr
      where
        closeHan = runReaderT (closeHandleNoThrow h Input) ev
        consume = do
          bs <- await
          liftIO $ BS.hPut h bs
          consume
        hndlr e = do
          closeHan
          lift (runReaderT (handleException oops e) ev)
    oops = Just (HandleOopsie Writing Input)

-- | Creates a background thread that will consume to the given Handle
-- from the given Producer.  Takes ownership of the 'Handle' and
-- closes it when done.
backgroundSendToProcess
  :: MonadSafe m
  => Handle
  -> Producer ByteString (SafeT IO) ()
  -> ReaderT Env m ()
backgroundSendToProcess han prod = do
  ev <- ask
  let csmr = runReader (consumeToHandle han) ev
      act = runSafeT . runEffect $ prod >-> csmr
  _ <- lift $ background act
  return ()

-- | Creates a background thread that will produce from the given
-- Handle into the given Consumer.  Takes possession of the Handle and
-- closes it when done.
backgroundReceiveFromProcess
  :: MonadSafe m
  => HandleDesc
  -> Handle
  -> Consumer ByteString (SafeT IO) ()
  -> ReaderT Env m ()
backgroundReceiveFromProcess desc han csmr = do
  ev <- ask
  let prod = runReader (produceFromHandle desc han) ev
      act = runSafeT . runEffect $ prod >-> csmr
  _ <- lift $ background act
  return ()

-- | Creates a 'Consumer' that will pump what it consumes to the
-- appropriate mailbox.  Does not seal the mailbox; if the mailbox is
-- exhausted, it's already been sealed.
processPump
  :: (MonadCatch m, MonadIO m)
  => (a -> PC.STM Bool)
  -- ^ Reception mailbox
  -> Consumer a m ()
processPump toStdinMbox = go
  where
    go = do
      bs <- await
      res <- liftIO . PC.atomically . toStdinMbox $ bs
      if res then go else return ()

-- | Does everything necessary to run a 'Handle' that is created to a
-- process standard input.  Creates mailbox, runs background thread
-- that pumps data out of the mailbox and into the process standard
-- input, and returns a Consumer that consumes and places what it
-- consumes into the mailbox for delivery to the background process.
runInputHandle
  :: MonadSafe mi
  => Handle
  -> Env
  -> Consumer ByteString mi ()
runInputHandle inp ev = do
  (PC.Output toStdinMbox, fromStdinMbox) <- newMailboxToProcess
  runReaderT (backgroundSendToProcess inp fromStdinMbox) ev
  processPump toStdinMbox

-- | Creates a 'Producer' that will pump from the given input source.
-- Does not seal the mailbox; if the source fails, it has already been
-- sealed.
processPuller
  :: (MonadCatch m, MonadIO m)
  => PC.STM (Maybe a)
  -- ^ Where the process delivers new ByteStrings
  -> Producer a m ()
processPuller fromOutMbox = go
  where
    go = do
      mayBs <- liftIO $ PC.atomically fromOutMbox
      case mayBs of
        Just bs -> yield bs >> go
        Nothing -> return ()

-- | Does everything necessary to run a 'Handle' that is created to a
-- process standard output or standard error.  Creates mailbox, runs
-- background thread that pumps data from the process output 'Handle'
-- into the mailbox, and returns a Producer that produces what comes
-- into the mailbox.
runOutputHandle
  :: MonadSafe mo
  => HandleDesc
  -> Handle
  -> Env
  -> Producer ByteString mo ()
runOutputHandle desc out ev = do
  (toStdoutMbox, PC.Input fromStdoutMbox) <- newMailboxFromProcess
  runReaderT (backgroundReceiveFromProcess desc out toStdoutMbox) ev
  processPuller fromStdoutMbox


-- * Creating subprocesses


-- | Creates a subprocess.  Registers destroyers for each handle
-- created, as well as for the ProcessHandle.
createProcess
  :: (MonadSafe m, MonadCatch (Base m))
  => Process.CreateProcess
  -> ReaderT Env m (Maybe Handle, Maybe Handle, Maybe Handle)
createProcess cp = mask $ \restore -> do
  (mayIn, mayOut, mayErr, han) <- liftIO $ Process.createProcess cp
  ev <- ask
  let close mayHan desc = maybe (return ())
        (\h -> runReaderT (closeHandleNoThrow h desc) ev) mayHan
  _ <- register (close mayIn Input)
  _ <- register (close mayOut Output)
  _ <- register (close mayErr Error)
  _ <- register (runReaderT (terminateProcess han) ev)
  mayHanVar <- asks envProcessHandle
  case mayHanVar of
    Nothing -> return ()
    Just hanVar -> liftIO $ putMVar hanVar han
  restore $ return (mayIn, mayOut, mayErr)


-- | Convenience wrapper for 'createProcess'.  The subprocess is
-- terminated and all its handles destroyed when the 'MonadSafe'
-- computation completes.
runCreateProcess
  :: (MonadSafe m, MonadCatch (Base m))
  => Maybe NonPipe
  -- ^ Standard input
  -> Maybe NonPipe
  -- ^ Standard output
  -> Maybe NonPipe
  -- ^ Standard error
  -> CreateProcess
  -> m (Maybe Handle, Maybe Handle, Maybe Handle, Env)
runCreateProcess inp out err cp = do
  let ev = envFromCreateProcess cp
  (inp', out', err') <- runReaderT
      (createProcess (convertCreateProcess inp out err cp)) ev
  return (inp', out', err', ev)

-- * Creating Proxy

-- | Do not create any 'Proxy' to or from the process.
pipeNone
  :: (MonadSafe m, MonadCatch (Base m))
  => NonPipe
  -- ^ Standard input
  -> NonPipe
  -- ^ Standard output
  -> NonPipe
  -- ^ Standard error
  -> CreateProcess
  -> m ()
pipeNone inp out err cp = flip runReaderT (envFromCreateProcess cp) $ do
  _ <- createProcess
    (convertCreateProcess (Just inp) (Just out) (Just err) cp)
  return ()

-- | Create a 'Consumer' for standard input.
pipeInput
  :: (MonadSafe m, MonadCatch (Base m))
  => NonPipe
  -- ^ Standard output
  -> NonPipe
  -- ^ Standard error
  -> CreateProcess
  -> Consumer ByteString m ()
  -- ^ A 'Consumer' for standard input
pipeInput out err cp = do
  (Just inp, _, _, ev) <- runCreateProcess Nothing (Just out) (Just err) cp
  runInputHandle inp ev


-- | Create a 'Producer' for standard output.
pipeOutput
  :: (MonadSafe m, MonadCatch (Base m))
  => NonPipe
  -- ^ Standard input
  -> NonPipe
  -- ^ Standard error
  -> CreateProcess
  -> Producer ByteString m ()
  -- ^ A 'Producer' for standard output
pipeOutput inp err cp = do
  (_, Just out, _, ev) <- runCreateProcess (Just inp) Nothing (Just err) cp
  runOutputHandle Output out ev

-- | Create a 'Producer' for standard error.
pipeError
  :: (MonadSafe m, MonadCatch (Base m))
  => NonPipe
  -- ^ Standard input
  -> NonPipe
  -- ^ Standard output
  -> CreateProcess
  -> Producer ByteString m ()
  -- ^ A 'Producer' for standard error
pipeError inp out cp = do
  (_, _, Just err, ev) <- runCreateProcess (Just inp) (Just out) Nothing cp
  runOutputHandle Error err ev

-- | Create a 'Consumer' for standard input and a 'Producer' for
-- standard output.
pipeInputOutput
  :: (MonadSafe mi, MonadSafe mo, MonadSafe m, MonadCatch (Base m))
  => NonPipe
  -- ^ Standard error
  -> CreateProcess
  -> m (Consumer ByteString mi (), Producer ByteString mo ())
  -- ^ A 'Consumer' for standard input, a 'Producer' for standard
  -- output
pipeInputOutput err cp = do
  (Just inp, Just out, _, ev) <-
    runCreateProcess Nothing Nothing (Just err) cp
  return (runInputHandle inp ev, runOutputHandle Output out ev)

-- | Create a 'Consumer' for standard input and a 'Producer' for
-- standard error.
pipeInputError
  :: (MonadSafe mi, MonadSafe me, MonadSafe m, MonadCatch (Base m))
  => NonPipe
  -- ^ Standard output
  -> CreateProcess
  -> m (Consumer ByteString mi (), Producer ByteString me ())
  -- ^ A 'Consumer' for standard input, a 'Producer' for standard
  -- error
pipeInputError out cp = do
  (Just inp, _, Just err, ev) <-
    runCreateProcess Nothing (Just out) Nothing cp
  return (runInputHandle inp ev, runOutputHandle Error err ev)

-- | Create a 'Producer' for standard output and a 'Producer' for
-- standard error.
pipeOutputError
  :: (MonadSafe mo, MonadSafe me, MonadSafe m, MonadCatch (Base m))
  => NonPipe
  -- ^ Standard input
  -> CreateProcess
  -> m (Producer ByteString mo (), Producer ByteString me ())
  -- ^ A 'Producer' for standard output, a 'Producer' for standard
  -- error
pipeOutputError inp cp = do
  (_, Just out, Just err, ev) <-
    runCreateProcess (Just inp) Nothing Nothing cp
  return ( runOutputHandle Output out ev
         , runOutputHandle Error err ev
         )


-- | Create a 'Consumer' for standard input, a 'Producer' for standard
-- output, and a 'Producer' for standard error.
pipeInputOutputError
  :: ( MonadSafe mi, MonadSafe mo, MonadSafe me,
       MonadSafe m, MonadCatch (Base m))
  => CreateProcess
  -> m ( Consumer ByteString mi (), Producer ByteString mo (),
         Producer ByteString me ())
  -- ^ A 'Consumer' for standard input, a 'Producer' for standard
  -- output, a 'Producer' for standard error
pipeInputOutputError cp = do
  (Just inp, Just out, Just err, ev) <-
    runCreateProcess Nothing Nothing Nothing cp
  return ( runInputHandle inp ev
         , runOutputHandle Output out ev
         , runOutputHandle Error err ev
         )
