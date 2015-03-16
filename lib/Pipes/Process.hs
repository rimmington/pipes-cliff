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
--
-- * Any pipelines you create must be created entirely within the
-- shell.  Ideally this library would create individual Pipe
-- components, one for each process.  However, that turns out to be a
-- surprisingly difficult problem.  See
--
-- <https://groups.google.com/d/msg/haskell-pipes/JFfyquj5HAg/Lxz7p50JOh4J>
--
-- All this library can do is launch a single process (or, perhaps, a
-- shell pipeline using something like @sh -c@) and hook Pipes 'Proxy'
-- to each of the standard streams.  However, the 'Proxy' must be a
-- 'Producer' (for standard input) or a 'Consumer' (for standard
-- output or standard error).  This limits your flexibility a lot, but
-- it can still be useful.
module Pipes.Process
  ( -- * Types
    StdStream
  , inherit
  , useHandle
  , useProxy
  , StreamToInput
  , StreamFromOutput
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
  ( MonadIO, liftIO, yield,
    await, runEffect, (>->), Proxy, X, MonadTrans,
    Producer, Consumer )
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified System.Process as Process
import System.IO (Handle, hClose)
import qualified System.IO as IO
import qualified Control.Exception
import Control.Monad.Trans.Cont (ContT(ContT), evalContT)
import Control.Concurrent.Async (wait, Async, withAsync)

-- | Values of this type are used to determine how to handle each of
-- the standard streams: standard input, standard output, and standard
-- error.  See 'inherit', 'useHandle', and 'useProxy'.
data StdStream a' a b m
  = Inherit
  -- ^ Inherit the stream from the current process.
  | UseHandle Handle
  -- ^ Use the given handle for the stream.
  | UseProxy (Proxy a' a () b m ()) (m () -> IO ())
-- | Inherit the stream from the current process.
inherit :: StdStream a' a b IO
inherit = Inherit

-- | Use the given handle for the stream.
useHandle :: Handle -> StdStream a' a b IO
useHandle = UseHandle

-- | Use a Pipes 'Proxy'.  The type system will only allow you to
-- use a 'StreamToInput' or a 'StreamFromOutput', depending on
-- whether you are trying to handle standard input, standard output,
-- or standard error.
--
-- The type variables have the same meaning as in 'Proxy'.  There is
-- no type variable for @b'@ because this type will always be ().
-- Similarly, there is no type variable for the
-- return type; it is always ().
useProxy
  :: Proxy a' a () b m ()
  -- ^ The 'Proxy' to use.

  -> (m () -> IO ())
  -- A function that, when applied to a value in the base
  -- 'Monad' of the 'Proxy', returns an 'IO' action.  This allows you
  -- to use any base 'Monad' you wish, provided that it is an instance
  -- of 'MonadIO' and that you can pair it with an appropriate
  -- function.
  -> StdStream a' a b m
useProxy = UseProxy





-- | A 'StdStream' that can be used to stream to standard input.
type StreamToInput m = StdStream X () ByteString m

-- | A 'StdStream' that can be used to stream from standard output or
-- from standard error.
type StreamFromOutput m = StdStream () ByteString X m

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
  :: StdStream xa' xa xb xm
  -- ^ Stdin
  -> StdStream ya' ya yb ym
  -- ^ Stdout
  -> StdStream za' za zb zm
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
  :: StdStream a' a b m
  -> Process.StdStream
convertStdStream s = case s of
  Inherit -> Process.Inherit
  UseHandle h -> Process.UseHandle h
  UseProxy _ _ -> Process.CreatePipe

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
--
-- Any IO exceptions are caught but are ignored.  After an IO
-- exception, production ceases.
produceFromHandle
  :: MonadIO m
  => Handle
  -> Producer ByteString m ()
produceFromHandle h = liftIO get >>= go
  where
    get = Control.Exception.catch (fmap Just $ BS.hGetSome h bufSize)
      han
    han :: Control.Exception.IOException -> IO (Maybe a)
    han _ = return Nothing
    go Nothing = return ()
    go (Just bs)
      | BS.null bs = return ()
      | otherwise = yield bs >> produceFromHandle h


-- | Create a 'Consumer'' from a 'Handle'.  The 'Consumer' will put
-- each 'ByteString' it receives into the 'Handle'.  Does nothing to
-- close the handle at any time.
--
-- Any IO exceptions are caught but are ignored.  After an IO
-- exception, production ceases.
consumeToHandle
  :: MonadIO m
  => Handle
  -> Consumer ByteString m ()
consumeToHandle h = do
  bs <- await
  let han :: Control.Exception.IOException -> IO (Maybe a)
      han _ = return Nothing
  putRes <- liftIO (Control.Exception.catch (fmap Just (BS.hPut h bs)) han)
  case putRes of
    Nothing -> return ()
    Just _ -> consumeToHandle h


-- | Launches and runs a subprocess to completion.  Is exception-safe;
-- if an exception is thrown, the subprocess will be terminated using
-- 'Process.terminateProcess' and the associated 'Handle's will be
-- closed.
runProcess
  :: (MonadIO im, MonadIO om, MonadIO em)
  => StreamToInput im
  -- ^ Produces bytes for the process standard input.
  -> StreamFromOutput om
  -- ^ Consumes bytes from the subprocess standard output.
  -> StreamFromOutput em
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
    getter han prod rnr = rnr $ runEffect $
      produceFromHandle han >-> prod
    pusher han prod rnr = rnr $ runEffect $
      prod >-> consumeToHandle han
    user inH outH errH = evalContT $ do
      aIn <- case inp of
        UseProxy prod rnr -> case inH of
          Nothing -> error "runProcess: error 1"
          Just h -> fmap Just . spawn $ pusher h prod rnr
        _ -> return Nothing
      bOut <- case out of
        UseProxy prod rnr -> case outH of
          Nothing -> error "runProcess: error 2"
          Just h -> fmap Just . spawn $ getter h prod rnr
        _ -> return Nothing
      bErr <- case err of
        UseProxy prod rnr -> case errH of
          Nothing -> error "runProcess: error 3"
          Just h -> fmap Just . spawn $ getter h prod rnr
        _ -> return Nothing
      mayWait aIn
      mayWait bOut
      mayWait bErr

mayWait
  :: (MonadIO m, MonadTrans t, Monad (t m), MonadIO (t m))
  => Maybe (Async ()) -> t m ()
mayWait = maybe (return ()) (liftIO . wait)
