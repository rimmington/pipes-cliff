{-# LANGUAGE FlexibleContexts #-}

-- | This contains the innards of Cliff.  You probably won't need
-- anything that's in here; "Pipes.Cliff" re-exports the most useful
-- bindings.  But nothing will break if you use what's in here, so
-- it's here if you need it.
module Pipes.Cliff.CoreNew where

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
import Control.Concurrent.Async
import Control.Concurrent.MVar
import System.Exit
import qualified Control.Exception

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

-- | The two kinds of outbound handles.
data Outbound
  = Output
  | Error
  deriving (Eq, Ord, Show)

-- | Describes a handle.  From the perspective of the subprocess; for
-- example, 'Input' means that this handle is connected to the
-- process's standard input.

data HandleDesc
  = Input
  | Outbound Outbound
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
data Oopsie = Oopsie Activity HandleDesc CmdSpec IOException
  deriving (Eq, Show)

-- | Formats an 'Oopsie' for display.
renderOopsie
  :: String
  -- ^ The name of the currently runnning program
  -> Oopsie
  -> String
renderOopsie pn (Oopsie act desc cmd ioe) =
  pn ++ ": warning: when running command "
  ++ renderCommand cmd ++ ": " ++ renderHan
  ++ ": " ++ show ioe
  where
    renderCommand (ShellCommand str) = show str
    renderCommand (RawCommand fp ss)
      = concat . intersperse " " . map show
      $ fp : ss

    renderHan =
      "when " ++ actStr ++ " " ++ descStr
      where
        actStr = case act of
          Reading -> "reading from"
          Writing -> "writing to"
          Closing -> "closing the handle associated with"
        descStr = "standard " ++ case desc of
          Input -> "input"
          Outbound Output -> "output"
          Outbound Error -> "error"

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
-- the only field that is different is the 'handler' field.
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

  , handler :: Oopsie -> IO ()
  -- ^ Whenever an IO exception arises during the course of various
  -- IO actios, the exception is caught and placed into an 'Oopsie'
  -- that indicates why and where the exception happened.  The
  -- 'handler' determines what happens when an 'Oopsie' comes in.
  -- See 'Oopsie' for details.
  --
  -- The default 'handler' created by 'procSpec' is
  -- 'defaultHandler', which will simply print the exceptions to
  -- standard error.  You may not want to see the exceptions at all.
  -- For example, many exceptions come from broken pipes.  A broken
  -- pipe might be entirely normal in your circumstance.  For
  -- example, if you are streaming a large set of values to a pager
  -- such as @less@ and you expect that the user will often quit the
  -- pager without viewing the whole result, a broken pipe will
  -- result, which will print a warning message.  That can be a
  -- nuisance.
  --
  -- If you don't want to see the exceptions at all, just set
  -- 'handler' to 'squelch', which simply discards the exceptions.
  --
  -- Conceivably you could rig up an elaborate mechanism that puts
  -- the 'Oopsie's into a "Pipes.Concurrent" mailbox or something.
  -- Indeed, when using 'defaultHandler' each thread will print its
  -- warnings to standard error at any time.  If you are using
  -- multiple processes and each prints warnings at the same time,
  -- total gibberish can result as the text gets mixed in.  You
  -- could solve this by putting the errors into a
  -- "Pipes.Concurrent" mailbox and having a single thread print the
  -- errors; building this sort of functionality directly in to the
  -- library would clutter up the API somewhat so I have been
  -- reluctant to do it.
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

-- * ErrSpec

-- | Contains data necessary to deal with exceptions.
data ErrSpec = ErrSpec
  { esErrorHandler :: Oopsie -> IO ()
  , esCmdSpec :: CmdSpec
  }

makeErrSpec
  :: CreateProcess
  -> ErrSpec
makeErrSpec cp = ErrSpec
  { esErrorHandler = handler cp
  , esCmdSpec = cmdspec cp
  }

-- * ProcSpec

-- 'psExitCode' is here because you don't want to use
-- 'Process.waitForProcess' when there is more than one thread that
-- wants to do the waiting.  There is a comment about this in the
-- "System.Process" module.  See also
--
-- http://ghc.haskell.org/trac/ghc/ticket/9292
data ProcSpec = ProcSpec
  { psIn :: Maybe Handle
  , psOut :: Maybe Handle
  , psErr :: Maybe Handle
  , psErrSpec :: ErrSpec
  , psHandle :: ProcessHandle
  , psHandleLock :: MVar ()
  , psExitCode :: MVar ExitCode
  , psUsers :: Int
  }

waitOnHandle
  :: (MonadCatch m, MonadIO m)
  => ProcSpec
  -> m ExitCode
waitOnHandle ps = do
  newLock <- liftIO $ tryPutMVar (psHandleLock ps) ()
  if newLock
    then do
      code <- liftIO
        ( (Process.waitForProcess (psHandle ps))
          `Control.Exception.onException` (takeMVar (psHandleLock ps)))
      liftIO $ putMVar (psExitCode ps) code
      return code
    else (liftIO . takeMVar . psExitCode $ ps)


-- | Decrements the number of users.  If there are no users left,
-- also waits on the process.
decrementAndWaitIfLast
  :: (MonadCatch m, MonadIO m)
  => MVar (Either a ProcSpec)
  -> m ()
decrementAndWaitIfLast mv = decrement >>= f
  where
    decrement = do
      ei <- liftIO $ takeMVar mv
      let ps' = case ei of
            Left _ -> error "decrementAndWaitIfLast: MVar not initialized"
            Right g -> g { psUsers = psUsers g - 1 }
      liftIO $ putMVar mv (Right ps')
      return ps'
    f ps'
      | psUsers ps' == 0 = waitOnHandle ps' >> return ()
      | otherwise = return ()



-- * Exception handling

-- | Sends an exception using the exception handler specified in the
-- 'ErrSpec'.
handleException
  :: MonadIO m
  => Activity
  -> HandleDesc
  -> IOException
  -> ErrSpec
  -> m ()
handleException act desc exc ev = liftIO $ sender oops
  where
    spec = esCmdSpec ev
    sender = esErrorHandler ev
    oops = Oopsie act desc spec exc


-- | Run an action, taking all IO errors and sending them to the handler.
handleErrors
  :: (MonadCatch m, MonadIO m)
  => Activity
  -> HandleDesc
  -> ErrSpec
  -> m ()
  -> m ()
handleErrors activ desc ev act = catch act catcher
  where
    catcher e = liftIO $ hndlr oops
      where
        spec = esCmdSpec ev
        hndlr = esErrorHandler ev
        oops = Oopsie activ desc spec e


-- | Close a handle.  Catches any exceptions and passes them to the handler.
closeHandleNoThrow
  :: (MonadCatch m, MonadIO m)
  => Handle
  -> HandleDesc
  -> ErrSpec
  -> m ()
closeHandleNoThrow hand desc ev = handleErrors Closing desc
  ev (liftIO $ hClose hand)


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
-- 'MonadSafe' computation completes.
conveyor :: MonadSafe m => Effect (SafeT IO) () -> m ()
conveyor efct
  = (background . liftIO . runSafeT . runEffect $ efct) >> return ()


-- | A version of 'Control.Concurrent.Async.wait' with an overloaded
-- 'MonadIO' return type.  Allows you to wait for the return value of
-- threads launched with 'background'.  If the thread throws an
-- exception, 'waitForThread' will throw that same exception.
waitForThread :: MonadIO m => Async a -> m a
waitForThread = liftIO . wait


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

-- | Creates a new mailbox and returns 'Proxy' that stream values
-- into and out of the mailbox.  Each 'Proxy' is equipped with a
-- finalizer that will seal the mailbox immediately after production
-- or consumption has completed, even if such completion is not due
-- to an exhausted mailbox.  This will signal to the other side of
-- the mailbox that the mailbox is sealed.
newMailbox
  :: (MonadIO m, MonadSafe mi, MonadSafe mo)
  => m (Consumer a mi (), Producer a mo ())
newMailbox = do
  (toBox, fromBox, seal) <- liftIO $ PC.spawn' messageBuffer
  let csmr = register (liftIO $ PC.atomically seal)
             >> PC.toOutput toBox
      pdcr = register (liftIO $ PC.atomically seal)
             >> PC.fromInput fromBox
  return (csmr, pdcr)


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
  :: (MonadSafe m, MonadCatch (Base m))
  => HandleDesc
  -> Handle
  -> ErrSpec
  -> Producer ByteString m ()
produceFromHandle hDesc h ev = do
  _ <- register (closeHandleNoThrow h hDesc ev)
  let hndlr e = lift $ handleException Reading hDesc e ev
      produce = liftIO (BS.hGetSome h bufSize) >>= go
      go bs
        | BS.null bs = return ()
        | otherwise = yield bs >> produce
  produce `catch` hndlr
  

getProcSpec
  :: (MonadMask m, MonadIO m)
  => MVar (IO ProcSpec)
  -> MVar (Either IOException ProcSpec)
  -> m ProcSpec
getProcSpec mvAct mvSpec = mask_ $ do
  mayAct <- liftIO $ tryTakeMVar mvAct
  case mayAct of
    Nothing -> do
      ei <- liftIO $ readMVar mvSpec
      case ei of
        Left e -> throwM e
        Right g -> return g
    Just act -> do
      ei <- try . liftIO $ act
      case ei of
        Left e -> do
          liftIO $ putMVar mvSpec (Left e)
          throwM e
        Right g -> do
          liftIO $ putMVar mvSpec (Right g)
          return g

-- The finalizers in consumeToHandle are 'register'ed in a
-- particular order.  Currently SafeT will call these finalizers on
-- a LIFO basis; this implementation depends on that behavior.  That
-- way the handle is closed before the process is waited on.

consumeToHandle
  :: (MonadSafe m, MonadCatch (Base m))
  => MVar (IO ProcSpec)
  -> MVar (Either IOException ProcSpec)
  -> Consumer ByteString m ()
consumeToHandle mvAct mvSpec = mask $ \restore -> do
  spec <- getProcSpec mvAct mvSpec
  _ <- register (decrementAndWaitIfLast mvSpec)
  let han = case psIn spec of
        Nothing -> error "consumeToHandle: handle not initialized"
        Just h -> h
  _ <- register (closeHandleNoThrow han Input (psErrSpec spec))
  let hndlr e = handleException Writing Input e (psErrSpec spec)
      go = do
        bs <- await
        liftIO $ BS.hPut han bs
        go
  restore $ go `catch` hndlr


backgroundSendToProcess
  :: (MonadSafe m, MonadCatch (Base m))
  => MVar (IO ProcSpec)
  -> MVar (Either IOException ProcSpec)
  -> Producer ByteString (SafeT IO) ()
  -> m ()
backgroundSendToProcess mvAct mvSpec pdcr = do
  let effect = pdcr >-> consumeToHandle mvAct mvSpec
  _ <- background . runSafeT . runEffect $ effect
  return ()

-- | Creates a background thread that will produce from the given
-- Handle into the given Consumer.  Takes possession of the Handle and
-- closes it when done.
backgroundReceiveFromProcess
  :: MonadSafe m
  => HandleDesc
  -> Handle
  -> Consumer ByteString (SafeT IO) ()
  -> ErrSpec
  -> m ()
backgroundReceiveFromProcess desc han csmr ev = background act >> return ()
  where
    prod = produceFromHandle desc han ev
    act = runSafeT . runEffect $ prod >-> csmr

-- | Does everything necessary to run a 'Handle' that is created to a
-- process standard input.  Creates mailbox, runs background thread
-- that pumps data out of the mailbox and into the process standard
-- input, and returns a Consumer that consumes and places what it
-- consumes into the mailbox for delivery to the background process.
runInputHandle
  :: (MonadSafe m, MonadCatch (Base m))
  => MVar (IO ProcSpec)
  -> MVar (Either IOException ProcSpec)
  -> Consumer ByteString m Done
runInputHandle = undefined

{-
runInputHandle mvar = mask $ \restore -> do
  ps <- getProcSpec mvar
  let han = case psIn ps of
        Just h -> h
        Nothing -> error "runInputHandle: handle not found"
  restore $ do
    (toMbox, fromMbox) <- newMailbox
    backgroundSendToProcess mvar han fromMbox (psErrSpec ps)
    toMbox
  code <- liftIO . Process.waitForProcess . psHandle $ ps
  _ <- decrementUsers mvar
  restore . return $ Done (esCmdSpec . psErrSpec $ ps) code
-}

-- | Does everything necessary to run a 'Handle' that is created to a
-- process standard output or standard error.  Creates mailbox, runs
-- background thread that pumps data from the process output 'Handle'
-- into the mailbox, and returns a Producer that produces what comes
-- into the mailbox.
runOutputHandle
  :: (MonadSafe m, MonadCatch (Base m))
  => Outbound
  -> MVar (IO ProcSpec)
  -> MVar (Either IOException ProcSpec)
  -> Producer ByteString m Done
runOutputHandle = undefined
{-
runOutputHandle desc mvar = mask $ \restore -> do
  ps <- getProcSpec mvar
  let han = case desc of
        Output -> case psOut ps of
          Nothing -> error "runOutputHandle: no standard output found"
          Just h -> h
        Error -> case psErr ps of
          Nothing -> error "runOutputHandle: no standard error found"
          Just h -> h
        Input -> error "runOutputHandle: bad parameter"
  restore $ do
    (toMbox, fromMbox) <- newMailbox
    backgroundReceiveFromProcess desc han toMbox (psErrSpec ps)
    fromMbox
  code <- liftIO . Process.waitForProcess . psHandle $ ps
  _ <- decrementUsers mvar
  restore . return $ Done (esCmdSpec . psErrSpec $ ps) code
-}

-- * Creating subprocesses

createProcSpecMVar
  :: MonadIO m
  => Int
  -- ^ Number of users
  -> Maybe NonPipe
  -> Maybe NonPipe
  -> Maybe NonPipe
  -> CreateProcess
  -> m (MVar (IO ProcSpec))
createProcSpecMVar nUsers inp out err cp = liftIO $ newMVar act
  where
    act = do
      let cp' = convertCreateProcess inp out err cp
          es = ErrSpec (handler cp) (cmdspec cp)
      (inp', out', err', ph) <- Process.createProcess cp'
      mvarLock <- liftIO newEmptyMVar
      mvarEc <- liftIO newEmptyMVar
      return $ ProcSpec inp' out' err' es ph mvarLock mvarEc nUsers

-- * Creating Proxy

-- TODO change this to take the whole CreateProcess.  Also, add an
-- identifier to CreateProcess.
data Done = Done CmdSpec ExitCode
  deriving (Eq, Ord, Show)

-- | Create a 'Consumer' for standard input.
pipeInput
  :: (MonadIO m, MonadSafe mi, MonadCatch (Base mi))
  => NonPipe
  -- ^ Standard output
  -> NonPipe
  -- ^ Standard error
  -> CreateProcess
  -> m (Consumer ByteString mi Done)
  -- ^ A 'Consumer' for standard input
pipeInput out err cp = do
  mvAct <- createProcSpecMVar 1 Nothing (Just out) (Just err) cp
  mvSpec <- liftIO newEmptyMVar
  return $ runInputHandle mvAct mvSpec

-- | Create a 'Producer' for standard output.
pipeOutput
  :: (MonadIO m, MonadSafe mo, MonadCatch (Base mo))
  => NonPipe
  -- ^ Standard input
  -> NonPipe
  -- ^ Standard error
  -> CreateProcess
  -> m (Producer ByteString mo Done)
  -- ^ A 'Producer' for standard output
pipeOutput inp err cp = do
  mvAct <- createProcSpecMVar 1 (Just inp) Nothing (Just err) cp
  mvSpec <- liftIO newEmptyMVar
  return $ runOutputHandle Output mvAct mvSpec

-- | Create a 'Producer' for standard error.
pipeError
  :: (MonadIO m, MonadSafe me, MonadCatch (Base me))
  => NonPipe
  -- ^ Standard input
  -> NonPipe
  -- ^ Standard output
  -> CreateProcess
  -> m (Producer ByteString me Done)
  -- ^ A 'Producer' for standard error
pipeError inp out cp = do
  mvAct <- createProcSpecMVar 1 (Just inp) (Just out) Nothing cp
  mvSpec <- liftIO newEmptyMVar
  return $ runOutputHandle Error mvAct mvSpec

-- | Create a 'Consumer' for standard input and a 'Producer' for
-- standard output.
pipeInputOutput
  :: ( MonadIO m,
       MonadSafe mi, MonadCatch (Base mi),
       MonadSafe mo, MonadCatch (Base mo))
  => NonPipe
  -- ^ Standard error
  -> CreateProcess
  -> m (Consumer ByteString mi Done, Producer ByteString mo Done)
  -- ^ A 'Consumer' for standard input, a 'Producer' for standard
  -- output
pipeInputOutput err cp = do
  mvAct <- createProcSpecMVar 2 Nothing Nothing (Just err) cp
  mvSpec <- liftIO newEmptyMVar
  return $ ( runInputHandle mvAct mvSpec
           , runOutputHandle Output mvAct mvSpec )

-- | Create a 'Consumer' for standard input and a 'Producer' for
-- standard error.
pipeInputError
  :: ( MonadIO m,
       MonadSafe mi, MonadCatch (Base mi),
       MonadSafe me, MonadCatch (Base me))
  => NonPipe
  -- ^ Standard output
  -> CreateProcess
  -> m (Consumer ByteString mi Done, Producer ByteString me Done)
  -- ^ A 'Consumer' for standard input, a 'Producer' for standard
  -- error
pipeInputError out cp = do
  mvAct <- createProcSpecMVar 2 Nothing Nothing (Just out) cp
  mvSpec <- liftIO newEmptyMVar
  return $ ( runInputHandle mvAct mvSpec
           , runOutputHandle Error mvAct mvSpec )

-- | Create a 'Producer' for standard output and a 'Producer' for
-- standard error.
pipeOutputError
  :: ( MonadIO m,
       MonadSafe mo, MonadCatch (Base mo),
       MonadSafe me, MonadCatch (Base me))
  => NonPipe
  -- ^ Standard input
  -> CreateProcess
  -> m (Producer ByteString mo Done, Producer ByteString me Done)
  -- ^ A 'Producer' for standard input, a 'Producer' for standard
  -- error
pipeOutputError inp cp = do
  mvAct <- createProcSpecMVar 2 (Just inp) Nothing Nothing cp
  mvSpec <- liftIO newEmptyMVar
  return $ ( runOutputHandle Output mvAct mvSpec
           , runOutputHandle Error mvAct mvSpec )

-- | Create a 'Consumer' for standard input, a 'Producer' for standard
-- output, and a 'Producer' for standard error.
pipeInputOutputError
  :: ( MonadIO m,
       MonadSafe mi, MonadCatch (Base mi),
       MonadSafe mo, MonadCatch (Base mo),
       MonadSafe me, MonadCatch (Base me))
  => CreateProcess
  -> m ( Consumer ByteString mi Done,
         Producer ByteString mo Done,
         Producer ByteString me Done )
pipeInputOutputError cp = do
  mvAct <- createProcSpecMVar 3 Nothing Nothing Nothing cp
  mvSpec <- liftIO newEmptyMVar
  return $ ( runInputHandle mvAct mvSpec,
             runOutputHandle Output mvAct mvSpec,
             runOutputHandle Error mvAct mvSpec)
