{-# LANGUAGE RankNTypes #-}

-- | Create subprocesses and send data to and from them using Pipes.
--
-- All communication with the subprocess is conducted using strict
-- 'ByteString's.  If you're dealing with textual data, you must make
-- sure it is properly encoded; building a Pipe to encode a value from
-- the @Text@ library to a UTF-8 'ByteString' should be
-- straightforward.  I/O is performed using the functions in the
-- @ByteString@ library; since these functions ultimately rely on
-- 'System.IO.hGetBut' and 'System.IO.hPutBuf', it does not matter
-- whether a 'Handle' is in text mode or binary mode; nor does it
-- matter what the handle's encoding setting is or whether it does any
-- newline conversion.
--
-- Limitations:
--
-- * There is no way to truly interact with the subprocess.
-- Input is sent in, and output is read concurrently; however, there
-- is no way to read a portion of the subprocess output and then send
-- different input to the process as a result of the output that has
-- been read so far.
--
-- * Only deals with standard input, standard output, and standard
-- error; you cannot create additional streams to or from the process.
module Pipes.Process
  ( -- * Types
    RunProxy(..)
  , RunProducer
  , RunConsumer
  , StdStream(..)
  , Process.CmdSpec(..)
  , ProcSpec(..)
  , procSpec

  -- * Running a sub-process
  , runProcess

  -- * Creating 'Consumer'' and 'Producer'' from a 'Handle'
  , produceFromHandle
  , consumeToHandle
  ) where

import Control.Monad.IO.Class ()
import System.Exit (ExitCode)
import Pipes
  ( MonadIO, Producer', liftIO, yield, Consumer',
    await, runEffect, (>->), Proxy, X, MonadTrans)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified System.Process as Process
import System.IO (Handle, hClose)
import qualified System.IO as IO
import qualified Control.Exception
import Control.Monad.Trans.Cont (ContT(ContT), evalContT)
import Control.Concurrent.Async (wait, Async, withAsync)

-- # RunProxy

-- | 'RunProxy' bundles a 'Proxy' with a function that runs 'Effects'
-- that the 'Proxy' will ultimately produce.
--
-- The type variables have the same meaning as in 'Proxy'.
data RunProxy a' a b' b m r
  = RunProxy (Proxy a' a b' b m r) (m () -> IO ())
  -- ^ @RunProxy p f@, where
  --
  -- @p@ is a 'Proxy', and
  --
  -- @f@ is a function that, when applied to a value in the base
  -- 'Monad' of the 'Proxy', returns an 'IO' action.  This allows you
  -- to use any base 'Monad' you wish, provided that it is an instance
  -- of 'MonadIO' and that you can pair it with an appropriate
  -- function.

-- | A 'RunProxy' that produces 'ByteString'.
type RunProducer m = RunProxy X () () ByteString m ()

-- | A 'RunProxy' that consumes 'ByteString'.
type RunConsumer m = RunProxy () ByteString () X m ()

data StdStream a' a b' b m r
  = Inherit
  | UseHandle Handle
  | UseProxy (RunProxy a' a b' b m r)


-- | Specifies how to launch a process.  The "System.Process" module
-- is used to launch the process.
data ProcSpec = ProcSpec
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
  }


convertProcSpec
  :: StdStream xa' xa xb' xb xm xr
  -- ^ Stdin
  -> StdStream ya' ya yb' yb ym yr
  -- ^ Stdout
  -> StdStream za' za zb' zb zm zr
  -- ^ Stderr
  -> ProcSpec
  -> Process.CreateProcess
convertProcSpec inp out err ps = Process.CreateProcess
  { Process.cmdspec = cmdspec ps
  , Process.cwd = cwd ps
  , Process.env = env ps
  , Process.std_in = convertStdStream inp
  , Process.std_out = convertStdStream out
  , Process.std_err = convertStdStream err
  , Process.close_fds = close_fds ps
  , Process.create_group = create_group ps
  , Process.delegate_ctlc = delegate_ctlc ps
  }

convertStdStream
  :: StdStream a' a b' b m r
  -> Process.StdStream
convertStdStream s = case s of
  Inherit -> Process.Inherit
  UseHandle h -> Process.UseHandle h
  UseProxy _ -> Process.CreatePipe

-- | Creates a 'ProcSpec' with the given program name and arguments.
-- In addition:
--
-- * 'cwd' is 'Nothing'
--
-- * 'env' is 'Nothing'
--
-- * 'close_fds' is 'True'
--
-- * 'create_group' is 'False'
--
-- * 'delegate_ctlc' is 'False'
procSpec
  :: String
  -> [String]
  -> ProcSpec
procSpec p a = ProcSpec
  { cmdspec = Process.RawCommand p a
  , cwd = Nothing
  , env = Nothing
  , close_fds = True
  , create_group = False
  , delegate_ctlc = False
  }

-- # Producers and consumers

-- # Running a sub-process

useProcess
  :: IO (Maybe Handle, Maybe Handle, Maybe Handle, Process.ProcessHandle)
  -> (Maybe Handle -> Maybe Handle -> Maybe Handle -> IO ())
  -> IO ExitCode
useProcess make use = Control.Exception.bracketOnError make rel run
  where
    rel (ih, oh, eh, han) = do
      Process.terminateProcess han
      _ <- Process.waitForProcess han
      closeNoThrow ih
      closeNoThrow oh
      closeNoThrow eh
    run tup = runHandles tup use


spawn :: IO a -> ContT b IO (Async a)
spawn io = ContT (withAsync io)

runHandles
  :: (Maybe Handle, Maybe Handle, Maybe Handle, Process.ProcessHandle)
  -> (Maybe Handle -> Maybe Handle -> Maybe Handle -> IO ())
  -> IO ExitCode
runHandles (inp, out, err, han) use = do
  use inp out err
  c <- Process.waitForProcess han
  closeNoThrow inp
  closeNoThrow out
  closeNoThrow err
  return c

-- | Closes a handle.  Suppresses all IO exceptions; others are thrown.
closeNoThrow :: Maybe Handle -> IO ()
closeNoThrow mayH = Control.Exception.catch (maybe (return ()) hClose mayH)
  han
  where
    han :: Control.Exception.IOException -> IO ()
    han _ = return ()


-- | I have no idea what this should be.  I'll start with a simple
-- small value and see how it works.
bufSize :: Int
bufSize = 1024


-- | Create a 'Producer'' from a 'Handle'.  The 'Producer'' will get
-- 'ByteString' from the 'Handle' and produce them.  Does nothing to
-- close the given 'Handle' at any time.
produceFromHandle
  :: MonadIO m
  => Handle
  -> Producer' ByteString m ()
produceFromHandle h = liftIO (BS.hGetSome h bufSize) >>= go
  where
    go bs | BS.null bs = return ()
          | otherwise = yield bs >> produceFromHandle h

-- | Create a 'Consumer'' from a 'Handle'.  The 'Consumer' will put
-- each 'ByteString' it receives into the 'Handle'.  Does nothing to
-- close the handle at any time.
consumeToHandle
  :: MonadIO m
  => Handle
  -> Consumer' ByteString m r
consumeToHandle h = do
  bs <- await
  liftIO $ BS.hPut h bs
  consumeToHandle h


-- | Launches and runs a subprocess to completion.  Is exception-safe;
-- if an exception is thrown, the subprocess will be terminated using
-- 'Process.terminateProcess' and the associated 'Handle's will be
-- closed.
runProcess
  :: (MonadIO xm, MonadIO ym, MonadIO zm)
  => StdStream X  ()         () ByteString xm ()
  -- ^ Produces bytes for the subprocess standard input.
  -> StdStream () ByteString () X          ym ()
  -- ^ Consumes bytes from the subprocess standard output.
  -> StdStream () ByteString () X          zm ()
  -- ^ Consumes bytes from the subprocess standard error.
  -> ProcSpec
  -> IO ExitCode
  -- ^ Returns the exit code from the subprocess, along with the
  -- witnesses from each stream.
runProcess inp out err ps = useProcess make user
  where
    make = do
      (i, o, e, h) <- Process.createProcess (convertProcSpec inp out err ps)
      let set = maybe (return ())
            (flip IO.hSetBuffering IO.NoBuffering)
      set i
      set o
      set e
      return (i, o, e, h)
    getter han (RunProxy consmr runner) = runner $ runEffect $
      produceFromHandle han >-> consmr
    pusher han (RunProxy prodcr runner) = runner $ runEffect $
      prodcr >-> consumeToHandle han
    user inH outH errH = evalContT $ do
      aIn <- case inp of
        UseProxy rp -> case inH of
          Nothing -> error "runProcess: error 1"
          Just h -> fmap Just . spawn $ pusher h rp
        _ -> return Nothing
      bOut <- case out of
        UseProxy rp -> case outH of
          Nothing -> error "runProcess: error 2"
          Just h -> fmap Just . spawn $ getter h rp
        _ -> return Nothing
      bErr <- case err of
        UseProxy rp -> case errH of
          Nothing -> error "runProcess: error 3"
          Just h -> fmap Just . spawn $ getter h rp
        _ -> return Nothing
      mayWait aIn
      mayWait bOut
      mayWait bErr

mayWait
  :: (MonadIO m, MonadTrans t, Monad (t m), MonadIO (t m))
  => Maybe (Async ()) -> t m ()
mayWait = maybe (return ()) (liftIO . wait)
