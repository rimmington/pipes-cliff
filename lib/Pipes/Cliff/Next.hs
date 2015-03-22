module Pipes.Cliff.Next where

import Control.Monad
import Control.Exception (IOException)
import qualified Control.Exception
import System.IO
import qualified System.IO as IO
import System.Environment
import qualified System.Process as Process
import System.Process (ProcessHandle)
import Pipes
import Pipes.Safe
import qualified Data.ByteString as BS
import qualified Pipes.Concurrent as PC
import Data.ByteString (ByteString)
import Control.Concurrent.MVar
import System.Exit
import Control.Concurrent.Async
import Control.Monad.Trans.Reader

-- # Exception handling

-- | Run an action, taking all IO errors and sending them to the handler.
handleErrors
  :: (MonadIO m, MonadCatch m)
  => CmdSpec
  -> Maybe HandleOopsie
  -> (Oopsie -> IO ())
  -> m ()
  -> m ()
handleErrors spec mayHandleOops han act = do
  eiR <- try act
  case eiR of
    Left e -> liftIO (han oops) >> return ()
      where
        oops = Oopsie mayHandleOops spec e
    Right () -> return ()


-- # Configuration and result types

-- | How will the subprocess get its information for this stream?
data NonPipe
  = Inherit
  -- ^ Use whatever stream that the parent process has.
  | UseHandle Handle
  -- ^ Use the given handle for input or output

-- | Like 'Process.CmdSpec' in "System.Process", but also has an
-- instance for 'Show'.
data CmdSpec
  = ShellCommand String
  | RawCommand FilePath [String]
  deriving (Eq, Ord, Show)

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

  , exitCode :: Maybe (MVar (Maybe ExitCode))
  , processHandle :: Maybe (MVar (Maybe Process.ProcessHandle))
  , handler :: Oopsie -> IO ()
  }

data HandleOopsie = HandleOopsie Activity HandleDesc
  deriving (Eq,Show)

data Activity
  = Reading
  | Writing
  | Closing
  deriving (Eq, Ord, Show)

-- | Describes a handle.  From the perspective of the subprocess.
data HandleDesc
  = Input
  | Output
  | Error
  deriving (Eq, Ord, Show)

convertCmdSpec :: CmdSpec -> Process.CmdSpec
convertCmdSpec (ShellCommand s) = Process.ShellCommand s
convertCmdSpec (RawCommand p ss) = Process.RawCommand p ss

data Oopsie = Oopsie (Maybe HandleOopsie) CmdSpec IOException
  deriving (Eq, Show)

defaultHandler :: Oopsie -> IO ()
defaultHandler = undefined

data Env = Env
  { envErrorHandler :: Oopsie -> IO ()
  , envExitCode :: Maybe (MVar ExitCode)
  , envProcessHandle :: Maybe (MVar ProcessHandle)
  , envCmdSpec :: CmdSpec
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
  { cmdspec = RawCommand prog args
  , cwd = Nothing
  , env = Nothing
  , close_fds = False
  , create_group = False
  , delegate_ctlc = False
  , quiet = False
  , exitCode = Nothing
  , processHandle = Nothing
  , handler = defaultHandler
  }

-- # Production and consumption

-- | I have no idea what this should be.  I'll start with a simple
-- small value and see how it works.
bufSize :: Int
bufSize = 1024

-- | A buffer that holds 10 messages.  I have no idea if this is the
-- ideal size.  Don't use an unbounded buffer, though, because with
-- unbounded producers an unbounded buffer will fill up your RAM.
messageBuffer :: PC.Buffer a
messageBuffer = PC.bounded 1

newMailbox
  :: (MonadSafe m, MonadSafe mi, MonadSafe mo)
  => m (Consumer a mi (), Producer a mo ())
newMailbox = bracket (liftIO $ PC.spawn' messageBuffer) rel use
  where
    rel (_, _, seal) = liftIO $ PC.atomically seal
    use (PC.Output sndr, PC.Input rcvr, seal) = return (csmr, prod)
      where
        prod = finally go (liftIO (PC.atomically seal))
          where
            go = do
              mayVal <- liftIO $ PC.atomically rcvr
              case mayVal of
                Nothing -> return ()
                Just val -> yield val >> go
        csmr = finally go (liftIO (PC.atomically seal))
          where
            go = do
              val <- await
              rslt <- liftIO $ PC.atomically (sndr val)
              if rslt then go else return ()

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
produceFromHandle hDesc h = do
  hndlr <- asks envErrorHandler
  spec <- asks envCmdSpec
  let catcher e = do
        handleErrors spec (Just (HandleOopsie Closing hDesc)) hndlr
          (liftIO $ hClose h)
        liftIO $ hndlr oops
        where
          oops = Oopsie (Just (HandleOopsie Reading hDesc)) spec e
      go bs
        | BS.null bs = return ()
        | otherwise = yield bs >> produce
      produce = liftIO (BS.hGetSome h bufSize) >>= go
  return $ catch produce catcher

acquire
  :: MonadSafe m
  => Base m a
  -> (a -> Base m ())
  -> m a
acquire acq rel = mask $ \restore -> do
  a' <- liftBase acq
  register (rel a')
  restore $ return a'


background
  :: MonadSafe m
  => IO a
  -> m (Async a)
background act = acquire (liftIO $ async act) (liftIO . cancel)


-- | Create a 'Consumer' that consumes from a 'Handle'.  Takes
-- ownership of the 'Handle'; closes it when the 'Consumer'
-- terminates.  If any IO errors arise either during consumption or
-- when the 'Handle' is closed, they are caught and passed to the
-- handler.
consumeToHandle
  :: MonadSafe m
  => Handle
  -> Reader Env (Consumer ByteString m ())
consumeToHandle h = do
  hndlr <- asks envErrorHandler
  spec <- asks envCmdSpec
  let catcher e = do
        handleErrors spec (Just (HandleOopsie Closing Input)) hndlr
          (liftIO $ hClose h)
        liftIO $ hndlr oops
        where
          oops = Oopsie (Just (HandleOopsie Writing Input)) spec e
      consume = do
        bs <- await
        liftIO $ BS.hPut h bs
        consume
  return $ catch consume catcher

-- | Creates a background thread that will consume to the given Handle
-- from the given Producer.
backgroundSendToProcess
  :: MonadSafe m
  => Handle
  -> Producer ByteString (SafeT IO) ()
  -> ReaderT Env m ()
backgroundSendToProcess han prod = do
  env <- ask
  let csmr = runReader (consumeToHandle han) env
      act = runSafeT . runEffect $ prod >-> csmr
  _ <- lift $ background act
  return ()

-- | Creates a background thread that will produce from the given
-- Handle into the given Consumer.
backgroundReceiveFromProcess
  :: MonadSafe m
  => HandleDesc
  -> Handle
  -> Consumer ByteString (SafeT IO) ()
  -> ReaderT Env m ()
backgroundReceiveFromProcess desc han csmr = do
  env <- ask
  let prod = runReader (produceFromHandle desc han) env
      act = runSafeT . runEffect $ prod >-> csmr
  _ <- lift $ background act
  return ()

-- | Creates a subprocess.  Registers destroyers for each handle
-- created, as well as for the ProcessHandle.
createProcess
  :: MonadSafe m
  => Process.CreateProcess
  -> ReaderT Env m (Maybe Handle, Maybe Handle, Maybe Handle, Process.ProcessHandle)
createProcess = undefined

{-
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

-}
