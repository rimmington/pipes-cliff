module Pipes.Proctee.Mailboxes where

import System.IO
import Pipes
import Pipes.Concurrent
import Control.Concurrent.Async
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified System.Process as Process
import qualified Control.Exception
import Control.Monad.Trans.Cont

data StdStream
  = Inherit
  | UseHandle Handle
  | Mailbox

data ProcSpec = ProcSpec
  { cmdspec :: Process.CmdSpec
    -- ^ Executable and arguments, or shell command
  , cwd :: Maybe FilePath
  -- ^ A new current working directory for the subprocess; if
  -- 'Nothing', use the calling process's working directory.
  , env :: Maybe [(String, String)]
  -- ^ The environment for the subprocess; if 'Nothing', use the
  -- calling process's working directory.

  , std_in :: StdStream
  , std_out :: StdStream
  , std_err :: StdStream

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
  }

-- Waiting on a process handle more than once will merely return the
-- same code more than once.  See the source code for System.Process.

-- | Use a Process.  Do not return any of the acquired resources in
-- the result, as they will be destroyed.
useProcess
  :: ProcSpec
  -> ContT r IO ( Maybe Handle
                , Maybe Handle
                , Maybe Handle
                , Process.ProcessHandle)
useProcess ps = ContT f
  where
    f = Control.Exception.bracket acq rel
    acq = Process.createProcess (convertProcSpec ps)
    rel (i, o, e, h)= do
      Process.terminateProcess h
      _ <- Process.waitForProcess h
      let shut = maybe (return ()) (ignoreIOExceptions . hClose)
      shut i >> shut o >> shut e
      return ()

-- | Runs a particular action, ignoring all IO errors.  Sometimes
-- using hClose will result in a broken pipe error.  Since the process
-- may have already been shut down, this is to be expected.  Since
-- there is nothing that can really be done to respond to any IO error
-- that results from closing a handle, just ignore these errors.
ignoreIOExceptions :: IO () -> IO ()
ignoreIOExceptions a = Control.Exception.catch a f
  where
    f :: Control.Exception.IOException -> IO ()
    f _ = return ()

-- | Create a 'Producer'' from a 'Handle'.  The 'Producer'' will get
-- 'ByteString' from the 'Handle' and produce them.  Does nothing to
-- close the given 'Handle' at any time.
--
-- Any IO exceptions are caught but are ignored.  After an IO
-- exception, production ceases.
produceFromHandle
  :: MonadIO m
  => Handle
  -> Producer ByteString m ()
produceFromHandle h = liftIO (BS.hGetSome h bufSize) >>= go
  where
    go bs
      | BS.null bs = return ()
      | otherwise = yield bs >> produceFromHandle h


-- | Create a 'Consumer'' from a 'Handle'.  The 'Consumer' will put
-- each 'ByteString' it receives into the 'Handle'.  Does nothing to
-- close the handle at any time.
consumeToHandle
  :: MonadIO m
  => Handle
  -> Consumer ByteString m a
consumeToHandle h = do
  bs <- await
  liftIO $ BS.hPut h bs
  consumeToHandle h

-- | Creates a mailbox; seals it when done.
withMailbox :: ContT r IO (Output a, Input a)
withMailbox = ContT f
  where
    f calc = Control.Exception.bracket acq rel use
      where
        acq = spawn' messageBuffer
        rel (_, _, seal) = atomically seal
        use (out, inp, _) = calc (out, inp)


-- | A buffer that holds 10 messages.  I have no idea if this is the
-- ideal size.  Don't use an unbounded buffer, though, because with
-- unbounded producers an unbounded buffer will fill up your RAM.
messageBuffer :: Buffer a
messageBuffer = bounded 10

-- | Given a Handle that provides standard input to a process, return
-- an Output that can be used to put messages in for the standard
-- input.  Spawns another thread to feed to the process's standard
-- input.  Do not attempt to use the Output once you return out of the
-- ContT, as it will not work.

makeOutputBox
  :: Handle
  -> ContT r IO (Output ByteString)
makeOutputBox han = do
  (output, input) <- withMailbox
  let act = runEffect $ fromInput input >-> consumeToHandle han
  _ <- background act
  return output

        
-- | Given a Handle that provides standard output or standard error
-- from a process, return an Input that can be used to receive
-- messages.
makeInputBox
  :: Handle
  -> ContT r IO (Input ByteString)
makeInputBox han = do
  (output, input) <- withMailbox
  let act = runEffect $ produceFromHandle han >-> toOutput output
  _ <- background act
  return input

makeBox
  :: Maybe Handle
  -> (Handle -> ContT r IO b)
  -> ContT r IO (Maybe b)
makeBox mayHan mkr = case mayHan of
  Nothing -> return Nothing
  Just h -> fmap Just $ mkr h

makeBoxes
  :: (Maybe Handle, Maybe Handle, Maybe Handle)
  -> ContT r IO ( Maybe (Output ByteString)
                , Maybe (Input ByteString)
                , Maybe (Input ByteString)
                )
makeBoxes (mayIn, mayOut, mayErr) = do
  inp <- makeBox mayIn makeOutputBox
  out <- makeBox mayOut makeInputBox
  err <- makeBox mayErr makeInputBox
  return (inp, out, err)

data Mailboxes = Mailboxes
  { mbxStdIn :: Maybe (Output ByteString)
  , mbxStdOut :: Maybe (Input ByteString)
  , mbxStdErr :: Maybe (Input ByteString)
  , mbxHandle :: Process.ProcessHandle
  }

-- | Launch and use a subprocess.
--
-- /Warning/ - do not attempt to use any of the resources created by
-- the process after leaving the 'ContT' monad.  They are all
-- destroyed.  So any Pipes you create using the 'Output' or 'Input'
-- that connect to the process must finish their IO before you leave
-- the 'ContT' monad.  It's okay to return the 'System.Exit.ExitCode'
-- that you get from running the process, or any data you get from the
-- process--you just can't return something that must perform IO to
-- interact with the process.
--
-- Also, by default, exiting the 'ContT' monad
-- immediately destroys all subprocesses.  If you want to make sure
-- the process terminates first, use 'Process.waitForProcess' on the
-- handle which you can get from 'wireHandle' before leaving the
-- 'ContT' monad.
--
-- The upside of this warning is that because all subprocess resources
-- are destroyed after leaving the 'ContT' monad, this function is
-- exception safe.
--
-- If you want a safe interface which helps prevent you from shooting
-- yourself in the foot here, use only the 'runCreatedProcess'
-- function to run your processes.
createProcess
  :: ProcSpec
  -> ContT r IO Mailboxes
createProcess spec = do
  (inp, out, err, han) <- useProcess spec
  (inp', out', err') <- makeBoxes (inp, out, err)
  return $ Mailboxes inp' out' err' han

-- | Runs created processes, in a safe way.  No resources will leak
-- from the computation even if exceptions are thrown.  You will have
-- to use the process entirely within the 'ContT' monad, which will
-- prevent you from trying to use resources after they have been
-- destroyed.
--
-- Because 'IO' is involved, I can think of ways to circumvent the
-- safety that this function provides (you could, for instance, use an
-- @IORef@ or an @MVar@ to sneak values out of the 'ContT'
-- computation) but this function does prevent you from making
-- mistakes due to inadvertence.
runCreatedProcess :: ContT () IO a -> IO ()
runCreatedProcess = flip runContT (const (return ()))

-- | I have no idea what this should be.  I'll start with a simple
-- small value and see how it works.
bufSize :: Int
bufSize = 1024

-- | Runs a thread in the background.  Be sure to use this for each
-- 'Effect' if you need to run multiple 'Effect's that form part of a
-- pipeline; if you have a pipeline but you only start one thread at a
-- time, you may get a deadlock.  The 'Async' that is returned allows
-- you to later kill the thread if you need to.  The associated thread
-- will be killed when the entire 'ContT' computation completes; to
-- prevent this from happening, use 'wait'.

background :: IO a -> ContT r IO (Async a)
background = ContT . withAsync

convertProcSpec :: ProcSpec -> Process.CreateProcess
convertProcSpec a = Process.CreateProcess
  { Process.cmdspec = cmdspec a
  , Process.cwd = cwd a
  , Process.env = env a
  , Process.std_in = conv . std_in $ a
  , Process.std_out = conv . std_out $ a
  , Process.std_err = conv . std_err $ a
  , Process.close_fds = close_fds a
  , Process.create_group = create_group a
  , Process.delegate_ctlc = delegate_ctlc a
  }
  where
    conv = convertStdStream

convertStdStream :: StdStream -> Process.StdStream
convertStdStream a = case a of
  Inherit -> Process.Inherit
  UseHandle h -> Process.UseHandle h
  Mailbox -> Process.CreatePipe

procSpec
  :: String
  -> [String]
  -> ProcSpec
procSpec prog args = ProcSpec
  { cmdspec = Process.RawCommand prog args
  , cwd = Nothing
  , env = Nothing
  , std_in = Inherit
  , std_out = Inherit
  , std_err = Inherit
  , close_fds = False
  , create_group = False
  , delegate_ctlc = False
  }
