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
-- For example, to stream an infinite list of numbers to @less@, try
-- running this in @ghci@:
--
-- >>> import Pipes.Subprocess
-- >>> import Pipes
-- >>> import qualified Data.ByteString.Char8 as Char8
-- >>> let produceNums = each . map (Char8.snoc '\n' . Char8.pack . show) $ [0..]
-- >>> runProcess (RunProxy produceNums id) toStdout toStderr (procSpec "less" [])
--
-- Since @less@ lazily reads its input, and because Pipes produces a
-- lazy stream, you can stream an infinite set of values.  After you
-- quit out of @less@, 'runProcess' returns (among other things) the
-- exit code from @less@.
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
module Pipes.Subprocess
  ( -- * Types
    RunProxy(..)
  , RunProducer
  , RunConsumer
  , RunProducer'
  , RunConsumer'
  , ProcSpec(..)
  , procSpec

  -- * Running a sub-process
  , runProcess

  -- * Creating values of type 'RunProxy'
  , fromStdin
  , toStdout
  , toStderr
  , produceFromFile
  , consumeWriteToFile
  , consumeAppendToFile

  -- * Creating 'Consumer'' and 'Producer'' from a 'Handle'
  , produceFromHandle
  , consumeToHandle
  ) where

import Control.Monad.IO.Class ()
import System.Exit (ExitCode)
import Pipes
  ( MonadIO, Producer', liftIO, yield, Consumer',
    await, runEffect, (>->), lift, Proxy, X)
import Pipes.Safe (SafeT)
import qualified Pipes.Safe as Safe
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified System.Process as Process
import System.IO (Handle, hClose, stdin, stdout, stderr)
import qualified System.IO as IO
import qualified Control.Exception
import Control.Monad.Trans.Cont (ContT(ContT), evalContT)
import Control.Concurrent.Async (wait, Async, withAsync)

-- # RunProxy

-- | 'RunProxy' bundles a 'Proxy' with a function that runs 'Effects'
-- that the 'Proxy' will ultimately produce.
--
-- Type variables:
--
-- @a'@ - 'Proxy' upstream values out
--
-- @a@ - 'Proxy' upstream values in
--
-- @b'@ - 'Proxy' downstream values in
--
-- @b@ - 'Proxy' downtream values out
--
-- @m@ - 'Proxy' base monad
--
-- @r@ - 'Proxy' return type
--
-- @c@ - return type of the IO action that results from applying the
-- function you give below to the value in the base monad that results
-- from running this 'Proxy'.  Values of this type are known as the
-- /witness/.
data RunProxy a' a b' b m r c
  = RunProxy (Proxy a' a b' b m r) (m r -> IO c)
  -- ^ @RunProxy p f@, where
  --
  -- @p@ is a 'Proxy', and
  --
  -- @f@ is a function that, when applied to a value in the base
  -- 'Monad' of the 'Proxy', returns an 'IO' action.  This allows you
  -- to use any base 'Monad' you wish, provided that it is an instance
  -- of 'MonadIO' and that you can pair it with an appropriate
  -- function.

-- | A 'RunProxy' that produces 'ByteString'.  The 'Proxy' return type
-- and the witness type are both ().
type RunProducer m = RunProxy X () () ByteString m () ()

-- | A 'RunProxy' that consumes 'ByteString'.  The 'Proxy' return type
-- and the witness type are both ().
type RunConsumer m = RunProxy () ByteString () X m () ()

-- | Like 'RunProducer' but is polymorphic in the 'Proxy' return type
-- and the witness type.
type RunProducer' m r c = forall x' x. RunProxy x' x () ByteString m r c

-- | Like 'RunConsumer' but is polymorphic in the 'Proxy' return type
-- and the witness type.
type RunConsumer' m r c = forall y' y. RunProxy () ByteString y' y m r c


-- | Specifies how to launch a process.  The "System.Process" module
-- is used to launch the process.  All processes are created using
-- 'System.Process.RawCommand'.  The standard input, standard output,
-- and standard error will be opened with pipes (that is, using
-- 'System.Process.CreatePipe') so that you can communicate using a
-- 'Proxy'.
data ProcSpec = ProcSpec
  { program :: String
  -- ^ The name of the program.
  , args :: [String]
  -- ^ Any arguments to the program
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
  :: ProcSpec
  -> Process.CreateProcess
convertProcSpec ps = Process.CreateProcess
  { Process.cmdspec = Process.RawCommand (program ps) (args ps)
  , Process.cwd = cwd ps
  , Process.env = env ps
  , Process.std_in = Process.CreatePipe
  , Process.std_out = Process.CreatePipe
  , Process.std_err = Process.CreatePipe
  , Process.close_fds = close_fds ps
  , Process.create_group = create_group ps
  , Process.delegate_ctlc = delegate_ctlc ps
  }

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
  { program = p
  , args = a
  , cwd = Nothing
  , env = Nothing
  , close_fds = True
  , create_group = False
  , delegate_ctlc = False
  }

-- # Producers and consumers

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

-- | A 'RunProducer'' that will produce values from whatever file or
-- device is hooked to the standard input of the current process.  The
-- device will not be closed when input is done.
fromStdin :: RunProducer' IO () ()
fromStdin = RunProxy (produceFromHandle stdin) id

-- | A 'RunConsumer'' that will output values to whatever file or
-- device is hooked to the standard output of the current process.
-- The device will not be closed when output is done.
toStdout :: RunConsumer' IO () ()
toStdout = RunProxy (consumeToHandle stdout) id

-- | A 'RunConsumer'' that will output values to whatever file or
-- device is hooked to the standard error of the current process.  The
-- device will not be closed when output is done.
toStderr :: RunConsumer' IO () ()
toStderr = RunProxy (consumeToHandle stderr) id

-- | Creates a 'RunProducer' that will produce values from the file at
-- the given path.  The file handle will be closed when input is
-- complete, and it will also be closed if an exception is thrown.
produceFromFile :: FilePath -> RunProducer (SafeT IO)
produceFromFile file = RunProxy pxy Safe.runSafeT
  where
    pxy = Safe.bracket
      (IO.openFile file IO.ReadMode)
      IO.hClose
      produceFromHandle


-- | Creates a 'RunConsumer' that will output values to the file at
-- the given path.  Values are appended to the end of the file if it
-- already exists.  The file handle will be closed when input is
-- complete, and it will also be closed if an exception is thrown.
consumeAppendToFile :: FilePath -> RunConsumer (SafeT IO)
consumeAppendToFile file = RunProxy pxy Safe.runSafeT
  where
    pxy = Safe.bracket
      (IO.openFile file IO.AppendMode)
      IO.hClose
      consumeToHandle


-- | Creates a 'RunConsumer' that will output values to the file at
-- the given path.  The file is overwritten if it already exists.  The
-- file handle will be closed when input is complete, and it will also
-- be closed if an exception is thrown.
consumeWriteToFile :: FilePath -> RunConsumer (SafeT IO)
consumeWriteToFile file = RunProxy pxy Safe.runSafeT
  where
    pxy = Safe.bracket
      (IO.openFile file IO.WriteMode)
      IO.hClose
      consumeToHandle


-- # Running a sub-process

useProcess
  :: ProcSpec
  -> (Handle -> Handle -> Handle -> IO a)
  -> IO (a, ExitCode)
useProcess ps use = Control.Exception.bracketOnError get rel run
  where
    get = do
      (Just inp, Just out, Just err, han) <- Process.createProcess
        (convertProcSpec ps)
      return (inp, out, err, han)
    rel (inp, out, err, han) = do
      Process.terminateProcess han
      _ <- Process.waitForProcess han
      hClose inp
      hClose out
      hClose err
    run tup = runHandles tup use

runHandles
  :: (Handle, Handle, Handle, Process.ProcessHandle)
  -> (Handle -> Handle -> Handle -> IO a)
  -> IO (a, ExitCode)
runHandles (inp, out, err, han) use = do
  r <- use inp out err
  c <- Process.waitForProcess han
  hClose inp
  hClose out
  hClose err
  return (r, c)


spawn :: IO a -> ContT b IO (Async a)
spawn io = ContT (withAsync io)

-- There is no (easy) way to make the type of the RunConsumer values
-- polymorphic in the return tpe.  That is, you can't
--
-- RunConsumer' ByteString xm () xc
--
-- into
--
-- RunConsumer' ByteString xm xr xc
--
-- This is because 'produceFromHandle' needs to have some type it
-- returns when it is done yielding.  Most obviously this should be
-- ().
--
-- The type of RunProducer' does not have this problem; therefore it
-- is polymorphic in the return type.  Theoretically runProcess' could
-- return the final return type. As a practical matter though I
-- currently see no reason for it to do this; it seems that typical
-- Pipes usage is to use other monads in the stack to do things like
-- return values accumulated in a stateful way.  So for now, although
-- the stdin producer is polymorphic in its return type, this doesn't
-- give you anything useful.

-- | Launches and runs a subprocess to completion.  Is exception-safe;
-- if an exception is thrown, the subprocess will be terminated using
-- 'Process.terminateProcess' and the associated 'Handle's will be
-- closed.
runProcess
  :: (MonadIO xm, MonadIO ym, MonadIO zm)
  => RunProducer' xm xr xc
  -- ^ Produces bytes for the subprocess standard input.
  -> RunConsumer' ym () yc
  -- ^ Consumes bytes from the subprocess standard output.
  -> RunConsumer' zm () zc
  -- ^ Consumes bytes from the subprocess standard error.
  -> ProcSpec
  -> IO (ExitCode, xc, yc, zc)
  -- ^ Returns the exit code from the subprocess, along with the
  -- witnesses from each stream.
runProcess inpP outP errP spec
  = fmap conv $ useProcess spec user
  where
    conv ((xc, yc, zc), cd) = (cd, xc, yc, zc)
    getter han (RunProxy consmr runner) = runner $ runEffect $
      produceFromHandle han >-> consmr
    pusher han (RunProxy prodcr runner) = runner $ runEffect $
      prodcr >-> consumeToHandle han
    user inH outH errH = evalContT $ do
      aIn <- spawn (pusher inH inpP)
      aOut <- spawn (getter outH outP)
      aErr <- spawn (getter errH errP)
      xc <- lift $ wait aIn
      yc <- lift $ wait aOut
      zc <- lift $ wait aErr
      return (xc, yc, zc)

