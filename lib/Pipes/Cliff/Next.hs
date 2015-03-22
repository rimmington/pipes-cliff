module Pipes.Cliff.Next where

import Control.Monad
import qualified Control.Exception
import System.IO
import qualified System.IO as IO
import System.Environment
import qualified System.Process as Process
import System.Process (ProcessHandle)
import Pipes
import Pipes.Safe
import qualified Data.ByteString as BS
import Pipes.Concurrent
import Data.ByteString (ByteString)
import Control.Concurrent.MVar
import System.Exit
import Control.Concurrent.Async
import Control.Monad.Trans.Reader

-- # Exception handling

-- | Runs a particular action.  Any IO errors are caught; a warning
-- message is printed if we're not being quiet, and 'Nothing' is
-- returned.  Otherwise, the result is returned.

catchAndWarn
  :: (MonadCatch m, MonadIO m)
  => Bool
  -- ^ Be quiet?
  -> String
  -- ^ What are we doing?  Used for warning message.
  -> m a
  -- ^ Do this
  -> m (Maybe a)
catchAndWarn beQuiet desc act
  = Pipes.Safe.catch (liftM Just act) hndle
  where
    hndle e = do
      let _types = e :: Control.Exception.IOException
      when (not beQuiet) . liftIO $ do
        pn <- getProgName
        let msg = pn ++ ": warning: caught exception when "
              ++ desc ++ ": " ++ show e
        IO.hPutStrLn IO.stderr msg
      return Nothing

-- | Runs a particular action, ignoring all IO errors.  Sometimes
-- using hClose will result in a broken pipe error.  Since the process
-- may have already been shut down, this is to be expected.  Since
-- there is nothing that can really be done to respond to any IO error
-- that results from closing a handle, just ignore these errors.
ignoreIOExceptions :: Bool -> String -> IO () -> IO ()
ignoreIOExceptions beQuiet desc a = Control.Exception.catch a f
  where
    f :: Control.Exception.IOException -> IO ()
    f exc = when (not beQuiet) $ do
      pn <- getProgName
      let msg = pn ++ ": warning: ignoring exception caught when " ++ desc
                ++ ": " ++ show exc
      IO.hPutStrLn IO.stderr msg
      return ()

-- | Close a handle, ignoring all IO exceptions.
closeHandle
  :: MonadIO m
  => Bool
  -- ^ Be quiet?
  -> Handle
  -> m ()
closeHandle q h = liftIO $ ignoreIOExceptions q "closing handle" (hClose h)

-- | Terminate a process, ignoring all IO exceptions.
terminateProcess
  :: MonadIO m
  => Bool
  -- ^ Be quiet?
  -> Process.ProcessHandle
  -> m ()
terminateProcess q h = liftIO . ignoreIOExceptions q "terminating process" $ do
  _ <- Process.terminateProcess h
  _ <- Process.waitForProcess h
  return ()


-- # Configuration and result types

-- | How will the subprocess get its information for this stream?
data NonPipe
  = Inherit
  -- ^ Use whatever stream that the parent process has.
  | UseHandle Handle
  -- ^ Use the given handle for input or output

-- | Like 'System.Process.CreateProcess' in "System.Process",
-- this gives the necessary information to create a subprocess.  All
-- but one of these fields is also present in
-- 'System.Process.CreateProcess', and they all have the same meaning;
-- the only field that is different is the 'quiet' field.
data CreateProcess = CreateProcess
  { cmdspec :: Process.CmdSpec
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

  , quiet :: Bool
  -- ^ If True, does not print messages to standard error when IO
  -- exceptions arise when closing handles or terminating processes.
  -- Sometimes these errors arise due to broken pipes; this can be
  -- normal, depending on the circumstances.  For example, if you
  -- are streaming a large set of values to a pager such as @less@
  -- and you expect that the user will often quit the pager without
  -- viewing the whole result, a broken pipe will result, which will
  -- print a warning message.  That can be a nuisance.  If you don't
  -- want to see these errors, set 'quiet' to 'True'.

  , exitCode :: Maybe (MVar ExitCode)
  , processHandle :: Maybe (MVar Process.ProcessHandle)
  }

data Env = Env
  { envQuiet :: Bool
  , envExitCode :: Maybe (MVar ExitCode)
  , envProcessHandle :: Maybe (MVar ProcessHandle)
  }

convertCreateProcess
  :: Maybe NonPipe
  -> Maybe NonPipe
  -> Maybe NonPipe
  -> CreateProcess
  -> Process.CreateProcess
convertCreateProcess inp out err a = Process.CreateProcess
  { Process.cmdspec = cmdspec a
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

convertNonPipe :: Maybe NonPipe -> Process.StdStream
convertNonPipe a = case a of
  Nothing -> Process.CreatePipe
  Just Inherit -> Process.Inherit
  Just (UseHandle h) -> Process.UseHandle h

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
-- * 'quiet' is 'False'
--
-- * 'exitCode' is 'Nothing'
--
-- * 'processHandle' is 'Nothing'

procSpec
  :: String
  -- ^ The name of the program to run, such as @less@.
  -> [String]
  -- ^ Command-line arguments
  -> CreateProcess
procSpec prog args = CreateProcess
  { cmdspec = Process.RawCommand prog args
  , cwd = Nothing
  , env = Nothing
  , close_fds = False
  , create_group = False
  , delegate_ctlc = False
  , quiet = False
  , exitCode = Nothing
  , processHandle = Nothing
  }


-- # Pipes

-- | I have no idea what this should be.  I'll start with a simple
-- small value and see how it works.
bufSize :: Int
bufSize = 1024

-- | Create a 'Producer' from a 'Handle'.  The 'Producer' will get
-- 'ByteString' from the 'Handle' and produce them.  Does nothing to
-- close the given 'Handle' at any time.  If there is an IO error
-- while receiving data, the error is caught and production ceases.
-- The exception is not re-thrown.
produceFromHandle
  :: (MonadCatch m, MonadIO m)
  => Bool
  -- ^ Be quiet?
  -> Handle
  -> Producer ByteString m ()
produceFromHandle beQuiet h = catchAndWarn beQuiet desc act >>= go
  where
    desc = "receiving data from process"
    act = liftIO (BS.hGetSome h bufSize)
    go Nothing = return ()
    go (Just bs)
      | BS.null bs = return ()
      | otherwise = yield bs >> produceFromHandle beQuiet h


-- | Create a 'Consumer' from a 'Handle'.  The 'Consumer' will put
-- each 'ByteString' it receives into the 'Handle'.  Does nothing to
-- close the handle at any time.  If there is an IO error while
-- sending data, the error is caught and consumption ceases.  The
-- exception is not re-thrown.
consumeToHandle
  :: (MonadCatch m, MonadIO m)
  => Bool
  -- ^ Be quiet?
  -> Handle
  -> Consumer ByteString m ()
consumeToHandle beQuiet h = do
  bs <- await
  mayRes <- catchAndWarn beQuiet "sending data to process"
    (liftIO $ BS.hPut h bs)
  case mayRes of
    Nothing -> return ()
    Just _ -> consumeToHandle beQuiet h


-- | A buffer that holds 10 messages.  I have no idea if this is the
-- ideal size.  Don't use an unbounded buffer, though, because with
-- unbounded producers an unbounded buffer will fill up your RAM.
messageBuffer :: Buffer a
messageBuffer = bounded 1

-- | Acquires a resource and registers a finalizer.
initialize
  :: MonadSafe m
  => m a
  -> (a -> Base m ())
  -> m a
initialize make destroy = Pipes.Safe.mask $ \_ -> do
  thing <- make
  _ <- Pipes.Safe.register (destroy thing)
  return thing

-- | Creates a mailbox; seals it when done.
newMailbox
  :: MonadSafe m
  => m (Output a, Input a, STM ())
newMailbox =
  initialize (liftIO $ spawn' messageBuffer)
  (\(_, _, seal) -> liftIO $ atomically seal)

-- | Runs a thread in the background.  Initializes a finalizer that
-- will cancel the thread if it is still running when the
-- 'MonadSafe' computation completes.
background :: MonadSafe m => IO a -> m (Async a)
background action = initialize (liftIO $ async action) (liftIO . cancel)

-- | Creates a thread that will run in the background and pump
-- messages from the given mailbox to the process via its handle.
-- Closes the Handle when done.
processPump
  :: MonadSafe m
  => Bool
  -- ^ Quiet?
  -> Handle
  -> (Input ByteString, STM ())
  -> m ()
processPump beQuiet hndle (input, seal) = do
  let pumper = flip Control.Exception.finally cleanup .
        runEffect $
          fromInput input >-> consumeToHandle beQuiet hndle
  _ <- background pumper
  return ()
  where
    cleanup = liftIO $ do
      closeHandle beQuiet hndle
      atomically seal

-- | Creates a thread that will run in the background and pull
-- messages from the process and place them into the given mailbox.
-- Closes the handle when done.
processPull
  :: MonadSafe m
  => Bool
  -- ^ Quiet?
  -> Handle
  -> (Output ByteString, STM ())
  -- ^ Output box, paired with action to close the box.
  -> m ()
processPull beQuiet hndle (output, seal) = do
  let puller = flip Control.Exception.finally cleanup .
        runEffect $
          produceFromHandle beQuiet hndle >-> toOutput output
  _ <- background puller
  return ()
  where
    cleanup = liftIO $ do
      closeHandle beQuiet hndle
      atomically seal


-- | Creates a mailbox that sends messages to the given process, and
-- sets up and runs threads to pump messages to the process.
makeToProcess
  :: (MonadSafe mp, MonadSafe m)
  => Bool
  -- ^ Quiet?
  -> Handle
  -> m (Consumer ByteString mp ())
makeToProcess beQuiet hndle = do
  (out, inp, seal) <- newMailbox
  processPump beQuiet hndle (inp, seal)
  return $ toOutput out `Pipes.Safe.finally` (liftIO (atomically seal))

-- | Creates a mailbox that receives messages from the given process,
-- and sets up and runs threads to receive the messages and deliver
-- them to the mailbox.
makeFromProcess
  :: (MonadSafe m, MonadSafe mp)
  => Bool
  -- ^ Quiet?
  -> Handle
  -> m (Producer ByteString mp ())
makeFromProcess beQuiet hndle = do
  (out, inp, seal) <- newMailbox
  processPull beQuiet hndle (out, seal)
  return $ fromInput inp `Pipes.Safe.finally` (liftIO (atomically seal))


-- | Creates a subprocess.  Registers destroyers for each handle
-- created, as well as for the ProcessHandle.
createProcess
  :: MonadSafe m
  => Process.CreateProcess
  -> ReaderT Env m (Maybe Handle, Maybe Handle, Maybe Handle, Process.ProcessHandle)
createProcess cp
  = liftM snd $ initialize (liftM2 (,) (asks envQuiet) mkProc) destroy
  where
    mkProc = liftIO $ Process.createProcess cp
    destroy (beQuiet, (mayIn, mayOut, mayErr, han)) = do
      let close = maybe (return ()) (closeHandle beQuiet)
      close mayIn
      close mayOut
      close mayErr
      terminateProcess beQuiet han


-- | Runs in the background an effect, typically one that is moving
-- data from one process to another.  For examples of its usage, see
-- "Pipes.Cliff.Examples".  The associated thread is killed when the
-- 'SafeT' computation completes.
conveyor :: Effect (Pipes.Safe.SafeT IO) () -> Pipes.Safe.SafeT IO ()
conveyor efct
  = (background . liftIO . runSafeT . runEffect $ efct) >> return ()


waitAndDeliverExitCode
  :: MonadIO m
  => ProcessHandle
  -> Maybe (MVar ExitCode)
  -> m ()
waitAndDeliverExitCode han mayVar = do
  code <- liftIO $ Process.waitForProcess han
  case mayVar of
    Nothing -> return ()
    Just var -> liftIO $ putMVar var code

-- | A version of 'Control.Concurrent.Async.wait' with an overloaded
-- 'MonadIO' return type.  Allows you to wait for the return value of
-- threads launched with 'background'.  If the thread throws an
-- exception, 'waitForThread' will throw that same exception.
waitForThread :: MonadIO m => Async a -> m a
waitForThread = liftIO . wait

-- | Creating Proxy

pipeNone
  :: MonadSafe m
  => NonPipe
  -> NonPipe
  -> NonPipe
  -> CreateProcess
  -> Effect m ()
pipeNone = undefined

pipeInput
  :: MonadSafe m
  => NonPipe
  -> NonPipe
  -> CreateProcess
  -> Consumer ByteString m ()
pipeInput = undefined

pipeOutput
  :: MonadSafe m
  => NonPipe
  -> NonPipe
  -> CreateProcess
  -> Producer ByteString m ()
pipeOutput = undefined

pipeError
  :: MonadSafe m
  => NonPipe
  -> NonPipe
  -> CreateProcess
  -> Producer ByteString m ()
pipeError = undefined

pipeInputOutput
  :: MonadSafe m
  => NonPipe
  -> CreateProcess
  -> Pipe ByteString ByteString m ()
pipeInputOutput = undefined

pipeInputError
  :: MonadSafe m
  => NonPipe
  -> CreateProcess
  -> Pipe ByteString ByteString m ()
pipeInputError = undefined

pipeOutputErrorC
  :: MonadSafe m
  => NonPipe
  -> CreateProcess
  -> Producer (Either ByteString ByteString) m ()
pipeOutputErrorC = undefined

pipeOutputError
  :: MonadSafe m
  => NonPipe
  -> CreateProcess
  -> m (Producer ByteString m (), Producer ByteString m ())
pipeOutputError = undefined

pipeInputOutputErrorC
  :: MonadSafe m
  => CreateProcess
  -> Pipe ByteString (Either ByteString ByteString) m ()
pipeInputOutputErrorC = undefined

pipeInputOutputErrorSplitError
  :: MonadSafe m
  => CreateProcess
  -> m (Pipe ByteString ByteString m (), Producer ByteString m ())
pipeInputOutputErrorSplitError = undefined

pipeInputOutputErrorSplitOutput
  :: MonadSafe m
  => CreateProcess
  -> m (Pipe ByteString ByteString m (), Producer ByteString m ())
pipeInputOutputErrorSplitOutput = undefined

pipeInputOutputErrorSplitBoth
  :: MonadSafe m
  => CreateProcess
  -> m ( Consumer ByteString m ()
       , Producer ByteString m ()
       , Producer ByteString m ())
pipeInputOutputErrorSplitBoth = undefined

