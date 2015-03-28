{-# LANGUAGE FlexibleContexts #-}

-- | This contains the innards of Cliff.  You probably won't need
-- anything that's in here; "Pipes.Cliff" re-exports the most useful
-- bindings.  But nothing will break if you use what's in here, so
-- it's here if you need it.
--
-- Exit code and waiting for processes: as of base 4.7, there was a
-- bug in 'System.Process.waitForProcess' which may arise if you have
-- multiple threads waiting for a single process to finish.  Thus this
-- module is set up so that only one thread does the wait, and it
-- places the result in an MVar.  See
--
-- http://ghc.haskell.org/trac/ghc/ticket/9292
-- 
--
-- 
module Pipes.Cliff.Core where

import System.Environment
import Data.List (intersperse)
import Control.Exception (IOException)
import System.IO
import System.Process (ProcessHandle)
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
import Control.Monad

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
--
-- Side effects: gets the program name from the environment, and
-- prints the Oopsie to standard error.
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

  }

-- | Do not show or do anything with exceptions; useful to use as a
-- 'handler'.
--
-- Side effects: None.
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

-- * MVar types

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


-- | Takes an action and returns a new action.  If the action is
-- never called the argument action will never be executed, but if
-- it is called more than once, it will only be executed once.
--
-- Side effects: creates a 'Var'.  Returns an IO action that modifies
-- the contents of that 'Var'.
once :: (MonadMask m, MonadIO m, MonadCatch m) => m a -> m (m a)
once act = do
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

-- | Data that is computed once, after the process has been created.
-- After computation, this data does not change.
data Console = Console
  { csIn :: Maybe Handle
  , csOut :: Maybe Handle
  , csErr :: Maybe Handle
  , csHandle :: Process.ProcessHandle
  , csExitCode :: IO ExitCode
  , csLock :: Lock
  -- ^ If locked, new resources cannot be created.
  , csReleasers :: Var [IO ()]
  -- ^ Each time a resource is created, register a finalizer here.
  }
  

-- | Is this process still running?
--
-- Side effects: examines the process handle to see if it has yet
-- returned a value.  Does not block; should return immediately.
isStillRunning :: ProcessHandle -> IO Bool
isStillRunning h = fmap (maybe True (const False))
  (Process.getProcessExitCode h)

-- | All the shared properties of a set of Proxy.
data Panel = Panel
  { pnlCreateProcess :: CreateProcess
  , pnlConsole :: IO Console
  }
  
-- | Add a finalizer to the Panel.  When the finalizers are run, all
-- exceptions are ignored, except asynchronous exceptions, which are
-- masked.
addReleaser :: Panel -> IO () -> IO ()
addReleaser pnl rel = do
  cnsl <- liftIO $ pnlConsole pnl
  withLock (csLock cnsl) $
    modifyVar_ (csReleasers cnsl) (\ls -> return (rel : ls))

-- | Terminates a process.
destroyProcess
  :: Panel
  -> IO ()
destroyProcess pnl = mask_ $ do
  cnsl <- pnlConsole pnl
  withLock (csLock cnsl) $ do
    let runFnlzr fnl = fnl `MC.catch` catcher
        catcher e = return ()
          where _types = e :: Control.Exception.SomeException
    fnlzrs <- readVar (csReleasers cnsl)
    mapM_ runFnlzr fnlzrs

-- | Gets the exit code of the process that belongs to the 'Panel'.
-- Side effects: may block if process has not yet exited.
getExitCode :: Panel -> IO ExitCode
getExitCode pnl = pnlConsole pnl >>= csExitCode

-- | Creates a new Panel.
--
-- Side effects: creates an MVar; this MVar acts as a unique
-- identifier which allows weak references to work more effectively
-- (see the documentation in "System.Mem.Weak" for the 'Weak' type for
-- more details on this.)  Does not create the process right away;
-- instead, creates an IO action that, when run, will create the
-- process.  This IO action contains another IO action that, when run,
-- will return the process exit code.
--
-- In addition, the IO action will fork a simple thread that will
-- immediately wait for the process.  In effect, this means there is
-- immediately a thread that will wait for the process to exit.
-- Because this IO action was created with 'once', that means only
-- one thread ever does the @wait@, which avoids a bug in
-- "System.Process".
newPanel
  :: Maybe NonPipe
  -> Maybe NonPipe
  -> Maybe NonPipe
  -> CreateProcess
  -> IO Panel
newPanel inp out err cp = liftM2 Panel (return cp)
  (liftIO . once . liftIO $ act)
  where
    act = mask_ $ do
      (inp', out', err', han) <- liftIO $ Process.createProcess
        (convertCreateProcess inp out err cp)
      let killHan mayH desc = case mayH of
            Nothing -> return ()
            Just h -> closeHandleNoThrow h desc (cmdspec cp) (handler cp)
          destroyers =
            [ killHan inp' Input, killHan out' (Outbound Output),
              killHan err' (Outbound Error),
              liftIO $ Process.terminateProcess han ]
      getCode <- once $ liftIO (Process.waitForProcess han)
      _ <- liftIO $ PC.forkIO (getCode >> return ())
      lock <- newLock
      rlsrs <- newVar destroyers
      return $ Console inp' out' err' han getCode lock rlsrs
      

-- * Exception handling

-- | Sends an exception using the exception handler specified in the
-- 'ErrSpec'.  Side effects: transmits the 'Oopsie' to the right
-- place; the recipient of the 'Oopsie' might have additional side
-- effects.
handleException
  :: MonadIO m
  => Activity
  -> HandleDesc
  -> CmdSpec
  -> (Oopsie -> IO ())
  -> IOException
  -> m ()
handleException act desc spec sender exc = liftIO $ sender oops
  where
    oops = Oopsie act desc spec exc

-- | Close a handle.  Catches any exceptions and passes them to the handler.
closeHandleNoThrow
  :: (MonadCatch m, MonadIO m)
  => Handle
  -> HandleDesc
  -> CmdSpec
  -> (Oopsie -> IO ())
  -> m ()
closeHandleNoThrow hand desc spec hndlr
  = (liftIO $ hClose hand) `catch` (handleException Closing desc spec hndlr)


-- * Threads

-- | Runs a thread in the background.
background
  :: MonadIO m
  => IO a
  -> m (Async a)
background = liftIO . async

-- | Runs in the background an effect, typically one that is moving
-- data from one process to another.  For examples of its usage, see
-- "Pipes.Cliff.Examples".
conveyor :: MonadIO m => Effect (SafeT IO) a -> m (Async a)
conveyor = background . liftIO . runSafeT . runEffect


-- | A version of 'Control.Concurrent.Async.wait' with an overloaded
-- 'MonadIO' return type.  Allows you to wait for the return value of
-- threads launched with 'background'.  If the thread throws an
-- exception, 'waitForThread' will throw that same exception.
waitForThread :: MonadIO m => Async a -> m a
waitForThread = liftIO . wait

-- | Kills a thread.
killThread :: MonadIO m => Async a -> m ()
killThread = liftIO . cancel


-- * Effects

-- | Runs in the foreground an effect in the 'SafeT' monad.  Called \"tidy\" because
-- the use of the 'SafeT' monad ensures that the effect will not leave
-- any stray processes or threads laying around.
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

-- * Type synonyms

-- | Consumer that reads values for a process standard input.  Its
-- input value is described in 'Outstream'.  The result type is a
-- tuple @(a, b)@, where @a@ is the return code from the upstream
-- process, and @b@ is the return code from this process.  @a@ will be
-- Nothing if the downstream process terminated before the upstream
-- one, or @Just@ if the upstream process terminated first.  The
-- 'Consumer' process's process exit code is always available and is
-- returned in @b@.
type Stdin m a
  = Consumer (Either a ByteString) m (Maybe a, ExitCode)

-- | Producer of values from a process standard output or error.  'yield' a
-- @'Left' 'ExitCode'@ if the stream is done producing values, or a
-- @'Right' 'ByteString'@ if the stream is still producing values.
-- 'Outstream' is polymorphic in its return type, @r@, becasuse the
-- 'Outstream' never stops yielding values; instead, it just 'yield's
-- its exit code over and over again after the process terminates.

type Outstream r m a
  = Producer (Either a ByteString) m r

-- | Producer of values from a process standard output.
type Stdout r m a = Outstream r m a

-- | Producer of values from a process standard error.
type Stderr r m a = Outstream r m a


-- * 'Proxy' combinators


-- * Production from and consumption to 'Handle's

-- | I have no idea what this should be.  I'll start with a simple
-- small value and see how it works.
bufSize :: Int
bufSize = 1024


-- | Initialize a handle.  Returns a computation in the MonadSafe
-- monad.  That computation has a registered finalizer that will close
-- a particular handle that is found in the 'Panel'.  As a side
-- effect, the IO action creating the 'Panel' is viewed, meaning that
-- the process will launch if it hasn't already done so.
initHandle
  :: (MonadIO m, MonadMask m, MonadSafe mi, MonadCatch (Base mi))
  => HandleDesc
  -- ^ Used for error messages
  -> (Console -> Handle)
  -- ^ Fetch the handle to close from the 'Panel'.
  -> Panel
  -- ^ Has the 'Handle' that will be closed.
  -> (Handle -> mi a)
  -- ^ The remainder of the computation.
  -> m (mi a)
initHandle desc get pnl mkProxy = mask_ $ do
  cnsl <- liftIO . pnlConsole $ pnl
  return $ mask $ \restore ->
    let han = get cnsl
        fnlzr = closeHandleNoThrow han desc (cmdspec . pnlCreateProcess $ pnl)
          (handler . pnlCreateProcess $ pnl)
    in register fnlzr >> (restore $ mkProxy han)

-- Returns a Consumer for process standard input.
--
-- Side effects: Process is started if it isn't already.  The returned
-- computation will await values and passes them on to the process
-- standard input mailbox.  Any IO exceptions are caught, and
-- consumption terminates.
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
      let hndlr = handleException Writing Input
            (cmdspec . pnlCreateProcess $ pnl)
            (handler . pnlCreateProcess $ pnl)
          go = do
            bs <- await
            liftIO $ BS.hPut han bs
            go
      go `catch` hndlr

-- | Produce values from a process standard output.  Process is
-- started if it isn't already.
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
    fn han =
      let hndlr = handleException Reading (Outbound outb)
            (cmdspec . pnlCreateProcess $ pnl)
            (handler . pnlCreateProcess $ pnl)
          go bs
            | BS.null bs = return ()
            | otherwise = yield bs >> produce
          produce = liftIO (BS.hGetSome han bufSize) >>= go
      in produce `catch` hndlr


-- | Given an 'Async', waits for that thread to finish processing
-- values.  When it completes, wait for the process exit code.
finishProxy
  :: MonadIO m
  => Async ()
  -> Panel
  -> m ExitCode
finishProxy thread pnl = do
  _ <- liftIO $ wait thread
  liftIO $ getExitCode pnl
  
-- | Forwards only Right values; terminates on the first Left value
-- and returns its value.  Useful to forward the output of an
-- 'Outstream' to a pipeline that expects only 'ByteString's.
forwardRight :: Monad m => Pipe (Either a b) b m a
forwardRight = do
  ei <- await
  case ei of
    Left l -> return l
    Right r -> yield r >> forwardRight

-- | Forwards all values, after rewrapping them in a Right.  Useful to
-- convert a producer of 'ByteString' into a 'Producer' of 'Either'
-- which can be fed to a 'Stdin'.
wrapRight :: Monad m => Pipe a (Either l a) m r
wrapRight = do
  x <- await
  yield (Right x)
  wrapRight


-- | Takes all steps necessary to get a 'Consumer' for standard
-- input.  Sets up a mailbox, runs a conveyor in the background.  Then
-- receives streaming data, and then gets the process exit code.
runInputHandle
  :: (MonadSafe mi, MonadCatch (Base mi))
  => Panel
  -- ^
  -> IO (Stdin mi r)
  -- ^
runInputHandle pnl = mask_ $ do
  csmr <- consumeToHandle pnl
  (toBox, fromBox) <- newMailbox
  asyncId <- conveyor $ fromBox >-> csmr
  addReleaser pnl (cancel asyncId)
  let f upstream = do
        thisCode <- finishProxy asyncId pnl
        return $ case upstream of
          Left firstCode -> (Just firstCode, thisCode)
          Right _downstreamStopped -> (Nothing, thisCode)

  return $ ((fmap Left forwardRight) >-> fmap Right toBox) >>= f

-- | Takes all steps necessary to get a 'Producer' for standard
-- input.  Sets up a mailbox, runs a conveyor in the background.  Then
-- receives streaming data, and then gets the process exit code.
runOutputHandle
  :: (MonadSafe mi, MonadCatch (Base mi))
  => Outbound
  -- ^
  -> Panel
  -- ^
  -> IO (Outstream r mi ExitCode)
  -- ^
runOutputHandle outb pnl = mask_ $ do
  pdcFromHan <- produceFromHandle outb pnl
  (toBox, fromBox) <- newMailbox
  asyncId <- conveyor $ pdcFromHan >-> toBox
  addReleaser pnl (cancel asyncId)
  let f _ = do
        code <- finishProxy asyncId pnl
        forever (return (Left code))
  return $ (fromBox >-> wrapRight) >>= f


-- * Creating Proxy

-- | Create a 'Consumer' for standard input.
pipeInput
  :: (MonadSafe m, MonadCatch (Base m))

  => NonPipe
  -- ^ Standard output

  -> NonPipe
  -- ^ Standard error

  -> CreateProcess

  -> IO (Stdin m a, Panel)
  -- ^ A 'Consumer' for standard input
pipeInput out err cp = mask_ $ do
  pnl <- newPanel Nothing (Just out) (Just err) cp
  inp <- runInputHandle pnl
  return (inp, pnl)
    

-- | Create a 'Producer' for standard output.
pipeOutput
  :: (MonadSafe mo, MonadCatch (Base mo))

  => NonPipe
  -- ^ Standard input

  -> NonPipe
  -- ^ Standard error

  -> CreateProcess

  -> IO (Stdout r mo ExitCode, Panel)
  -- ^ A 'Producer' for standard output
pipeOutput inp err cp = mask_ $ do
  pnl <- newPanel (Just inp) Nothing (Just err) cp
  pdcr <- runOutputHandle Output pnl
  return (pdcr, pnl)


-- | Create a 'Producer' for standard error.
pipeError
  :: (MonadSafe me, MonadCatch (Base me))

  => NonPipe
  -- ^ Standard input

  -> NonPipe
  -- ^ Standard output

  -> CreateProcess

  -> IO (Stderr r me ExitCode, Panel)
  -- ^ A 'Producer' for standard error

pipeError inp out cp = mask_ $ do
  pnl <- newPanel (Just inp) (Just out) Nothing cp
  pdcr <- runOutputHandle Error pnl
  return (pdcr, pnl)

-- | Create a 'Consumer' for standard input and a 'Producer' for
-- standard output.
pipeInputOutput
  :: ( MonadSafe mi, MonadCatch (Base mi),
       MonadSafe mo, MonadCatch (Base mo))

  => NonPipe
  -- ^ Standard error

  -> CreateProcess

  -> IO ((Stdin mi a, Stdout r mo ExitCode), Panel)
  -- ^ A 'Consumer' for standard input, a 'Producer' for standard
  -- output

pipeInputOutput err cp = mask_ $ do
  pnl <- newPanel Nothing Nothing (Just err) cp
  csmr <- runInputHandle pnl
  pdcr <- runOutputHandle Output pnl
  return ((csmr, pdcr), pnl)

-- | Create a 'Consumer' for standard input and a 'Producer' for
-- standard error.
pipeInputError
  :: ( MonadSafe mi, MonadCatch (Base mi),
       MonadSafe me, MonadCatch (Base me))

  => NonPipe

  -- ^ Standard output
  -> CreateProcess

  -> IO ((Stdin mi a, Stderr r me ExitCode), Panel)
  -- ^ A 'Consumer' for standard input, a 'Producer' for standard
  -- error
pipeInputError out cp = do
  pnl <- newPanel Nothing (Just out) Nothing cp
  csmr <- runInputHandle pnl
  pdcr <- runOutputHandle Error pnl
  return ((csmr, pdcr), pnl)


-- | Create a 'Producer' for standard output and a 'Producer' for
-- standard error.
pipeOutputError
  :: ( MonadSafe mo, MonadCatch (Base mo),
       MonadSafe me, MonadCatch (Base me))

  => NonPipe
  -- ^ Standard input

  -> CreateProcess

  -> IO ((Stdout ro mo ExitCode, Stderr re me ExitCode), Panel)
  -- ^ A 'Producer' for standard output and a 'Producer' for standard
  -- error

pipeOutputError inp cp = do
  pnl <- newPanel (Just inp) Nothing Nothing cp
  pdcrOut <- runOutputHandle Output pnl
  pdcrErr <- runOutputHandle Error pnl
  return ((pdcrOut, pdcrErr), pnl)


-- | Create a 'Consumer' for standard input, a 'Producer' for standard
-- output, and a 'Producer' for standard error.
pipeInputOutputError
  :: ( MonadSafe mi, MonadCatch (Base mi),
       MonadSafe mo, MonadCatch (Base mo),
       MonadSafe me, MonadCatch (Base me))

  => CreateProcess

  -> IO ((Stdin mi a, Stdout ro mo ExitCode, Stderr re me ExitCode), Panel)
  -- ^ A 'Consumer' for standard input, a 'Producer' for standard
  -- output, and a 'Producer' for standard error

pipeInputOutputError cp = do
  pnl <- newPanel Nothing Nothing Nothing cp
  csmr <- runInputHandle pnl
  pdcrOut <- runOutputHandle Output pnl
  pdcrErr <- runOutputHandle Error pnl
  return ((csmr, pdcrOut, pdcrErr), pnl)
