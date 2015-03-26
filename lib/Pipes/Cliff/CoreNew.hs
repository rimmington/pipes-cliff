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
import Pipes
import Pipes.Safe
import qualified Data.ByteString as BS
import qualified Pipes.Concurrent as PC
import Data.ByteString (ByteString)
import Control.Concurrent.Async
import qualified Control.Concurrent.MVar as MV
import Control.Concurrent.MVar (MVar)
import System.Exit
import qualified Control.Exception
import qualified Control.Monad.Catch as MC
import Control.Monad.Reader

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

-- | Describes all IO exceptions.  The 'Oopsie' contains the
-- 'IOException' itself, along with the 'CmdSpec' that was running
-- when the exception occurred.
--
-- The exceptions that are caught and placed into an 'Oopsie' may
-- arise from reading data from or writing data to a 'Handle'.  In
-- these errors, the associated 'Producer' or 'Consumer' will
-- terminate (which may trigger various cleanup actions in the
-- 'MonadSafe' computation) but the exception itself is not
-- re-thrown; rather, it is passed to the 'handler'.  Similarly, an
-- exception may occur while closing a handle; these exceptions are
-- caught, not rethrown, and are passed to the 'handler'.  If an
-- exception arises when terminating a process (I'm not sure this is
-- possible) then it is also caught, not rethrown, and passed to the
-- 'handler'.
--
-- If an exception arises when creating a process--such as a command
-- not being found--the exception is /not/ caught, handled, or
-- passed to the 'handler'.  In addition, no exceptions are caught
-- if they originated during a 'Process.waitForProcess'.  (I can't
-- conceive of how any synchronous exceptions could arise from
-- 'Process.waitForProcess', but if they do, Cliff does not handle
-- them.)  Also, an 'Oopsie' is created only for an 'IOException';
-- no other exceptions of any kind are caught or handled.  However,
-- exceptions of any kind will still trigger appropriate cleanup
-- actions in the 'MonadSafe' computation.
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
  -- errors; this sort of thing could be built into the library but
  -- so far I haven't been motivated to do it.

  , procInfo :: Maybe (MVar ProcInfo)
  -- ^ Allows you to retrieve information about the process after it
  -- has launched.  See 'ProcInfo' for details.  Usually you will
  -- not need this; in that case, set this to 'Nothing'.  If you do
  -- want to get a 'ProcInfo', create an empty 'MVar' and place it
  -- in a 'Just' here.  The 'ProcInfo' will be supplied when the
  -- process starts running.
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
-- * 'handler' is 'defaultHandler'
--
-- * 'procInfo' is 'Nothing'

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
  , procInfo = Nothing
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

-- * MVar types
--

-- ** Overloaded MVar operations

newMVar :: MonadIO m => a -> m (MVar a)
newMVar a = liftIO (MV.newMVar a)

takeMVar :: MonadIO m => MVar a -> m a
takeMVar mv = liftIO (MV.takeMVar mv)

putMVar :: MonadIO m => MVar a -> a -> m ()
putMVar mv a = liftIO (MV.putMVar mv a)

readMVar :: MonadIO m => MVar a -> m a
readMVar mv = liftIO (MV.readMVar mv)

evaluate :: MonadIO m => a -> m a
evaluate v = liftIO (Control.Exception.evaluate v)

modifyMVar
  :: (MonadIO m, MonadCatch m, MonadMask m)
  => MVar a -> (a -> m (a, b)) -> m b
modifyMVar m io = mask $ \restore -> do
  a <- takeMVar m
  (a', b) <- restore (io a >>= evaluate) `MC.onException` putMVar m a
  putMVar m a'
  return b

modifyMVar_
  :: (MonadIO m, MonadCatch m, MonadMask m)
  => MVar a -> (a -> m a) -> m ()
modifyMVar_ m io = mask $ \restore -> do
  a <- takeMVar m
  a' <- restore (io a) `MC.onException` putMVar m a
  putMVar m a'

withMVar
  :: (MonadCatch m, MonadMask m, MonadIO m)
  => MVar a -> (a -> m b) -> m b
withMVar m io = mask $ \restore -> do
  a <- liftIO $ MV.takeMVar m
  b <- restore (io a) `MC.onException` liftIO (MV.putMVar m a)
  liftIO $ MV.putMVar m a
  return b

newEmptyMVar :: MonadIO m => m (MVar a)
newEmptyMVar = liftIO MV.newEmptyMVar

-- ** Lock

-- | Guarantees single-thread access
--
-- All MVar idioms thanks to Neil Mitchell:
-- <http://neilmitchell.blogspot.com/2012/06/flavours-of-mvar_04.html>
type Lock = MVar ()

newLock :: MonadIO m => m Lock
newLock = newMVar ()

withLock :: (MonadCatch m, MonadIO m, MonadMask m) => Lock -> m a -> m a
withLock x = withMVar x . const

-- ** Var

-- | Operates on mutable variables in thread-safe way.

type Var a = MVar a

newVar :: MonadIO m => a -> m (Var a)
newVar = newMVar

modifyVar
  :: (MonadMask m, MonadCatch m, MonadIO m)
  => Var a -> (a -> m (a, b)) -> m b
modifyVar = modifyMVar

modifyVar_
  :: (MonadMask m, MonadIO m, MonadCatch m)
  => Var a -> (a -> m a) -> m ()
modifyVar_ = modifyMVar_

readVar :: MonadIO m => Var a -> m a
readVar = readMVar

-- ** Barrier

-- | Starts with no value, is written to once, and is read one or
-- more times.
type Barrier a = MVar a

newBarrier :: MonadIO m => m (Barrier a)
newBarrier = newEmptyMVar

signalBarrier :: MonadIO m => Barrier a -> a -> m ()
signalBarrier = putMVar

waitBarrier :: MonadIO m => Barrier a -> m a
waitBarrier = readMVar

-- ** MVar abstractions

-- | A Shrine contains a value.  That value is computed only once,
-- after which point it does not change.
type Shrine m a = m (m a)

-- | Takes an action and returns a new action.  If the action is
-- never called the argument action will never be executed, but if
-- it is called more than once, it will only be executed once.
enshrine :: (MonadMask m, MonadIO m, MonadCatch m) => m a -> Shrine m a
enshrine act = do
  var <- newVar Nothing
  return $ join $ modifyVar var $ \v -> case v of
    Nothing -> do
      b <- newBarrier
      let r = do
            x <- act
            signalBarrier b x
            return x
      return (Just b, r)
    Just b -> return (Just b, waitBarrier b)

-- | Views the value in the Shrine.  This operation will block if
-- the value in the Shrine either has not been computed or is being
-- computed by another thread.
viewShrine :: Monad m => Shrine m a -> m a
viewShrine = join

-- 'csExitCode' is here because you don't want to use
-- 'Process.waitForProcess' when there is more than one thread that
-- wants to do the waiting.  There is a comment about this in the
-- "System.Process" module.  See also

-- http://ghc.haskell.org/trac/ghc/ticket/9292
-- | Data that is computed once, after the process has been created.
-- After computation, this data does not change.
data Console = Console
  { csIn :: Maybe Handle
  , csOut :: Maybe Handle
  , csErr :: Maybe Handle
  , csHandle :: Process.ProcessHandle
  , csExitCode :: Shrine IO ExitCode
  }

-- | A unique process identifier.  This is a simple wrapper around
-- the type with the same name in "System.Process".  There is a bug
-- in "System.Process":
--
-- <http://ghc.haskell.org/trac/ghc/ticket/9292>
--
-- and that bug would make itself known in a multi-threaded library
-- like Cliff, so this wrapper prevents the occurrence of this bug
-- by preventing the user from ever using
-- 'System.Process.waitForProcess'.  Instead, Cliff performs one
-- common wait for for every process, and the result is stored in a
-- mutable variable for later use.
newtype ProcessHandle = ProcessHandle Process.ProcessHandle

-- | Is this process still running?
isStillRunning :: ProcessHandle -> IO Bool
isStillRunning (ProcessHandle h) = fmap (maybe True (const False))
  (Process.getProcessExitCode h)

-- | Terminates a process.  A process might not respond to this; use
-- 'isStillRunning' to see if the process responded.
terminateProcess :: ProcessHandle -> IO ()
terminateProcess (ProcessHandle h) = Process.terminateProcess h


-- | All the shared properties of a set of Proxy.
data Panel = Panel
  { pnlCreateProcess :: CreateProcess
  , pnlConsole :: Shrine IO Console
  , pnlId :: MVar ()
  }

getExitCode :: Panel -> IO ExitCode
getExitCode pnl = do
  cnsl <- viewShrine . pnlConsole $ pnl
  viewShrine . csExitCode $ cnsl

newPanel
  :: MonadIO m
  => Maybe NonPipe
  -> Maybe NonPipe
  -> Maybe NonPipe
  -> CreateProcess
  -> m Panel
newPanel inp out err cp = liftM3 Panel (return cp) (return cnsl) newEmptyMVar
  where
    cnsl = enshrine act
    act = do
      (inp', out', err', han) <- liftIO $ Process.createProcess
        (convertCreateProcess inp out err cp)
      let getCode = enshrine $ liftIO (Process.waitForProcess han)
      _ <- liftIO $ PC.forkIO ((viewShrine  getCode) >> return ())
      _ <- case procInfo cp of
        Nothing -> return ()
        Just mv -> putMVar mv (ProcInfo (ProcessHandle han)
          (viewShrine getCode))
      return $ Console inp' out' err' han getCode
      

-- * Exception handling

-- | Sends an exception using the exception handler specified in the
-- 'ErrSpec'.
handleException
  :: MonadIO m
  => Activity
  -> HandleDesc
  -> IOException
  -> Panel
  -> m ()
handleException act desc exc pnl = liftIO $ sender oops
  where
    spec = cmdspec . pnlCreateProcess $ pnl
    sender = handler . pnlCreateProcess $ pnl
    oops = Oopsie act desc spec exc

-- | Run an action, taking all IO errors and sending them to the handler.
handleErrors
  :: (MonadCatch m, MonadIO m)
  => Activity
  -> HandleDesc
  -> Panel
  -> m ()
  -> m ()
handleErrors activ desc pnl act = catch act catcher
  where
    catcher e = liftIO $ hndlr oops
      where
        spec = cmdspec . pnlCreateProcess $ pnl
        hndlr = handler . pnlCreateProcess $ pnl
        oops = Oopsie activ desc spec e


-- | Close a handle.  Catches any exceptions and passes them to the handler.
closeHandleNoThrow
  :: (MonadCatch m, MonadIO m)
  => Handle
  -> HandleDesc
  -> Panel
  -> m ()
closeHandleNoThrow hand desc pnl = handleErrors Closing desc
  pnl (liftIO $ hClose hand)


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
conveyor :: MonadSafe m => Effect (SafeT IO) a -> m (Async a)
conveyor = background . liftIO . runSafeT . runEffect


-- | A version of 'Control.Concurrent.Async.wait' with an overloaded
-- 'MonadIO' return type.  Allows you to wait for the return value of
-- threads launched with 'background'.  If the thread throws an
-- exception, 'waitForThread' will throw that same exception.
waitForThread :: MonadIO m => Async a -> m a
waitForThread = liftIO . wait


-- * Effects

-- | Runs an effect in the 'SafeT' monad.  Called \"tidy\" because
-- the use of the 'SafeT' monad ensures that the effect will not
-- leave any stray processes or threads laying around.
tidyEffect :: (MonadIO m, MonadMask m) => Effect (SafeT m) a -> m a
tidyEffect = runSafeT . runEffect


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


-- The finalizers in consumeToHandle are 'register'ed in a
-- particular order.  Currently SafeT will call these finalizers on
-- a LIFO basis; this implementation depends on that behavior.  That
-- way the handle is closed before the process is waited on.

-- | Initialize a handle.
initHandle
  :: (MonadIO m, MonadMask m, MonadSafe mi, MonadCatch (Base mi))
  => HandleDesc
  -> (Console -> Handle)
  -> Panel
  -> (Handle -> mi a)
  -> m (mi a)
initHandle desc get pnl mkProxy = mask_ $ do
  cnsl <- liftIO . viewShrine . pnlConsole $ pnl
  let han = get cnsl
      fnlzr = closeHandleNoThrow han desc pnl
      fnlzr' = closeHandleNoThrow han desc pnl
  _ <- liftIO $ MV.mkWeakMVar (pnlId pnl) fnlzr
  return $ mask $ \restore -> do
    _ <- register fnlzr'
    restore $ mkProxy han

consumeToHandle
  :: ( MonadIO m, MonadIO mi, MonadCatch mi, MonadSafe mi,
       MonadCatch (Base mi), MonadMask m)
  => Panel
  -> m (Consumer ByteString mi ())
consumeToHandle pnl = initHandle Input get pnl fn
  where
    get cnsl = case csIn cnsl of
      Just h -> h
      Nothing -> error "consumeToHandle: handle not initialized"
    fn han = do
      let hndlr e = handleException Writing Input e pnl
          go = do
            bs <- await
            liftIO $ BS.hPut han bs
            go
      go `catch` hndlr

produceFromHandle
  :: ( MonadIO m, MonadIO mi, MonadCatch mi, MonadSafe mi,
       MonadCatch (Base mi), MonadMask m)
  => Outbound
  -> Panel
  -> m (Producer ByteString mi ())
produceFromHandle outb pnl = initHandle (Outbound outb) get pnl fn
  where
    get cnsl = case outb of
      Output -> case csOut cnsl of
        Nothing -> error "produceFromHandle: stdout not initialized"
        Just h -> h
      Error -> case csErr cnsl of
        Nothing -> error "produceFromHandle: stderr not initialized"
        Just h -> h
    fn han = do
      let hndlr e = handleException Reading (Outbound outb) e pnl
          go bs
            | BS.null bs = return ()
            | otherwise = yield bs >> produce
          produce = liftIO (BS.hGetSome han bufSize) >>= go
      produce `catch` hndlr


finishProxy
  :: MonadIO m
  => Async ()
  -> Panel
  -> m ExitCode
finishProxy thread pnl = do
  _ <- liftIO $ wait thread
  liftIO $ getExitCode pnl

runInputHandle
  :: (MonadSafe m, MonadSafe mi, MonadCatch (Base mi))
  => Panel
  -> Consumer ByteString (SafeT IO) ()
  -> m (Consumer ByteString mi ExitCode)
runInputHandle pnl csmToHan = do
  (toBox, fromBox) <- newMailbox
  asyncId <- conveyor $ fromBox >-> csmToHan
  return $ do
    toBox
    finishProxy asyncId pnl

runOutputHandle
  :: (MonadSafe m, MonadSafe mi, MonadCatch (Base mi))
  => Panel
  -> Producer ByteString (SafeT IO) ()
  -> m (Producer ByteString mi ExitCode)
runOutputHandle pnl pdcFromHan = do
  (toBox, fromBox) <- newMailbox
  asyncId <- conveyor $ pdcFromHan >-> toBox
  return $ do
    fromBox
    finishProxy asyncId pnl

-- | Information about a process that is running.
data ProcInfo = ProcInfo ProcessHandle (IO ExitCode)
  -- ^ @ProcInfo h a@, where
  --
  -- @h@ is a 'ProcessHandle' which can be used to terminate the
  -- process.  Usually this is not necessary but this can be handy
  -- if you want to ensure that all processes are managed
  -- deterministically.
  --
  -- @a@ is an IO action that will return the exit code.  This
  -- action will block until the process returns.  Usually you can
  -- obtain this information more elegantly using Pipes idioms, but
  -- sometimes that is difficult or impossible (particularly if the
  -- 'Proxy' is part of a larger pipeline).  There is a bug in
  -- "System.Process" related to waiting for processes:
  --
  -- <http://ghc.haskell.org/trac/ghc/ticket/9292>
  --
  -- Use of this function works around this bug.


-- * Creating Proxy

-- | Create a 'Consumer' for standard input.
pipeInput
  :: (MonadSafe m, MonadCatch (Base m))

  => NonPipe
  -- ^ Standard output

  -> NonPipe
  -- ^ Standard error

  -> CreateProcess

  -> Consumer ByteString m ExitCode
  -- ^ A 'Consumer' for standard input
pipeInput out err cp = mask $ \restore -> do
  pnl <- newPanel Nothing (Just out) (Just err) cp
  csmr <- consumeToHandle pnl
  restore . join $ runInputHandle pnl csmr
    

-- | Create a 'Producer' for standard output.
pipeOutput
  :: (MonadSafe m, MonadCatch (Base m))

  => NonPipe
  -- ^ Standard input

  -> NonPipe
  -- ^ Standard error

  -> CreateProcess

  -> Producer ByteString m ExitCode
  -- ^ A 'Producer' for standard output
pipeOutput inp err cp = mask $ \restore -> do
  pnl <- newPanel (Just inp) Nothing (Just err) cp
  pdcr <- produceFromHandle Output pnl
  restore . join $ runOutputHandle pnl pdcr


-- | Create a 'Producer' for standard error.
pipeError
  :: (MonadSafe m, MonadCatch (Base m))

  => NonPipe
  -- ^ Standard input

  -> NonPipe
  -- ^ Standard output

  -> CreateProcess

  -> Producer ByteString m ExitCode
  -- ^ A 'Producer' for standard error

pipeError inp out cp = mask $ \restore -> do
  pnl <- newPanel (Just inp) (Just out) Nothing cp
  pdcr <- produceFromHandle Error pnl
  restore . join $ runOutputHandle pnl pdcr


-- | Create a 'Consumer' for standard input and a 'Producer' for
-- standard output.
pipeInputOutput
  :: ( MonadIO m, MonadMask m,
       MonadSafe mi, MonadCatch (Base mi),
       MonadSafe mo, MonadCatch (Base mo))

  => NonPipe
  -- ^ Standard error

  -> CreateProcess

  -> m (Consumer ByteString mi ExitCode, Producer ByteString mo ExitCode)
  -- ^ A 'Consumer' for standard input, a 'Producer' for standard
  -- output

pipeInputOutput err cp = mask_ $ do
  pnl <- newPanel Nothing Nothing (Just err) cp
  csmr <- consumeToHandle pnl
  pdcr <- produceFromHandle Output pnl
  return (join $ runInputHandle pnl csmr, join $ runOutputHandle pnl pdcr)



-- | Create a 'Consumer' for standard input and a 'Producer' for
-- standard error.
pipeInputError
  :: ( MonadIO m, MonadMask m,
       MonadSafe mi, MonadCatch (Base mi),
       MonadSafe me, MonadCatch (Base me))

  => NonPipe

  -- ^ Standard output
  -> CreateProcess

  -> m (Consumer ByteString mi ExitCode, Producer ByteString me ExitCode)
  -- ^ A 'Consumer' for standard input, a 'Producer' for standard
  -- error
pipeInputError out cp = do
  pnl <- newPanel Nothing (Just out) Nothing cp
  csmr <- consumeToHandle pnl
  pdcr <- produceFromHandle Error pnl
  return
    ( join $ runInputHandle pnl csmr
    , join $ runOutputHandle pnl pdcr
    )
-- | Create a 'Producer' for standard output and a 'Producer' for
-- standard error.
pipeOutputError
  :: ( MonadIO m, MonadMask m,
       MonadSafe mo, MonadCatch (Base mo),
       MonadSafe me, MonadCatch (Base me))

  => NonPipe
  -- ^ Standard input

  -> CreateProcess

  -> m (Producer ByteString mo ExitCode, Producer ByteString me ExitCode)
  -- ^ A 'Producer' for standard output and a 'Producer' for standard
  -- error
  --
pipeOutputError inp cp = do
  pnl <- newPanel (Just inp) Nothing Nothing cp
  pdcrOut <- produceFromHandle Output pnl
  pdcrErr <- produceFromHandle Error pnl
  return
    ( join $ runOutputHandle pnl pdcrOut
    , join $ runOutputHandle pnl pdcrErr
    )

-- | Create a 'Consumer' for standard input, a 'Producer' for standard
-- output, and a 'Producer' for standard error.
pipeInputOutputError
  :: ( MonadIO m, MonadMask m,
       MonadSafe mi, MonadCatch (Base mi),
       MonadSafe mo, MonadCatch (Base mo),
       MonadSafe me, MonadCatch (Base me))

  => CreateProcess

  -> m ( Consumer ByteString mi ExitCode,
         Producer ByteString mo ExitCode,
         Producer ByteString me ExitCode )
  -- ^ A 'Consumer' for standard input, a 'Producer' for standard
  -- output, and a 'Producer' for standard error

pipeInputOutputError cp = do
  pnl <- newPanel Nothing Nothing Nothing cp
  csmr <- consumeToHandle pnl
  pdcrOut <- produceFromHandle Output pnl
  pdcrErr <- produceFromHandle Error pnl
  return
    ( join $ runInputHandle pnl csmr
    , join $ runOutputHandle pnl pdcrOut
    , join $ runOutputHandle pnl pdcrErr
    )




