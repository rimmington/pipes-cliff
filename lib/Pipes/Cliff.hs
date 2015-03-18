-- | Spawn subprocesses and interact with them using "Pipes.Concurrent"
--
-- The interface in this module deliberately resembles the interface
-- in "System.Process".  However, one consequence of this is that you
-- will not want to have unqualified names from this module and from
-- "System.Process" in scope at the same time.
--
-- As in "System.Process", you create a subprocess by creating a
-- 'CreateProcess' record and then applying 'createProcess' to that
-- record.  The difference is that 'createProcess' returns 'Mailboxes'
-- to you.  You can request the creation of an 'Output' corresponding
-- to the subprocess standard input, and an 'Input' for each of
-- standard output and standard error.  You then interact with the
-- subprocess using "Pipes" and "Pipes.Concurrent".
--
-- __Use the @-threaded@ GHC option__ when compiling your programs or
-- when using GHCi.  Internally, this module uses
-- 'System.Process.waitForProcess' from the "System.Process" module;
-- it's also quite likely that you will use this function when you
-- write code using this library.  As the documentation for
-- 'waitForProcess' states, you must use the @-threaded@ option to
-- prevent every thread in the system from suspending when you use
-- 'waitForProcess'.  So, if your program experiences deadlocks, be
-- sure you used the @-threaded@ option.
--
-- This module relies on the "Pipes", "Pipes.Concurrent", and
-- "System.Process" modules.  You will want to have basic familiarity
-- with what all of those modules do before using this module.
--
-- This module also uses the 'ContT' type constructor from
-- "Control.Monad.Trans.Cont" to help with resource management.  You
-- can use 'ContT' like you would use any other 'Monad'.  Values from
-- this module that use the 'ContT' type constructor will
-- automatically deallocate or destroy the resource when the 'ContT'
-- computation copmletes.  That means that all threads and
-- subprocesses that you create in the 'ContT' computation are
-- destroyed when the computation completes, even if excptions are
-- thrown.  This also means you must take care to not use any of the
-- resources outside of the 'ContT' computation.  The 'safeCliff'
-- function will help you with this; or, for less safety but more
-- flexibility, use 'evalContT' from "Control.Monad.Trans.Cont".  You
-- can use 'liftIO' to run IO computations as part of 'ContT'
-- computations.
--
-- All communcation with subprocesses is done with strict
-- 'ByteString's.  If you are dealing with textual data, the @text@
-- library has functions to convert a 'ByteString' to a @Text@; you
-- will want to look at @Data.Text.Encoding@.
--
-- Nobody would mistake this module for a shell; nothing beats the
-- shell as a language for starting other programs, as the shell is
-- designed for that.  This module allows you to perform simple
-- streaming with subprocesses without leaving the comfort of Haskell.
-- Take a look at the README.md file, which is distributed with the
-- tarball or is available at Github at
--
-- <https://github.com/massysett/pipes-cliff>
--
-- There you will find references to other libraries that you might
-- find more useful than this one.
--
-- For some simple examples, consult "Pipes.Cliff.Examples".
module Pipes.Cliff where
{-
  ( StdStream(..)
  , CreateProcess(..)
  , Mailboxes(..)
  , proc
  , createProcess
  , background
  , safeCliff

  -- * Re-exports
  -- $reexports
  , module Control.Concurrent.Async
  , module Control.Monad.Trans.Cont
  , module System.Process
  , module Pipes
  , module Pipes.Concurrent
  , module System.Exit
  ) where
-}
import System.IO
import Pipes
import Pipes.Concurrent
import qualified Control.Concurrent.Async (Async, withAsync)
import Control.Monad.Trans.Cont (ContT(..), evalContT)
import Control.Monad (liftM)
import Control.Concurrent.Async (wait, Async, async, cancel)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified System.Process as Process
import System.Process (waitForProcess)
import qualified Control.Exception
import Pipes.Safe
import System.Exit

-- # Exception handling

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


-- # Configuration and result types

-- | How will the subprocess get its information for this stream?
data StdStream
  = Inherit
  -- ^ Use whatever stream that the parent process has.
  | UseHandle Handle
  -- ^ Use the given handle for input or output
  | Mailbox
  -- ^ Set up a mailbox

-- | Like 'System.Process.CreateProcess' in "System.Process",
-- this gives the necessary information to create a subprocess.
data CreateProcess = CreateProcess
  { cmdspec :: Process.CmdSpec
    -- ^ Executable and arguments, or shell command
  , cwd :: Maybe FilePath
  -- ^ A new current working directory for the subprocess; if
  -- 'Nothing', use the calling process's working directory.
  , env :: Maybe [(String, String)]
  -- ^ The environment for the subprocess; if 'Nothing', use the
  -- calling process's working directory.

  , std_in :: StdStream
  -- ^ Use this kind of stream for standard input
  , std_out :: StdStream
  -- ^ Use this kind of stream for standard output
  , std_err :: StdStream
  -- ^ Use this kind of stream for standard error

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

data ToProcess = ToProcess (Output ByteString) ReleaseKey

data FromProcess = FromProcess (Input ByteString) ReleaseKey

-- | Contains any mailboxes that communicate with the subprocess,
-- along with the process handle.
data Mailboxes = Mailboxes

  { mbxStdIn :: Maybe ToProcess
  -- ^ Mailbox to send data to the standard input, if you requested
  -- such a mailbox using 'Mailbox' for 'std_in'.  Each 'ByteString'
  -- that you place into the 'Output' will be sent to the subprocess
  -- using 'BS.hPut'.

  , mbxStdOut :: Maybe FromProcess
  -- ^ Mailbox to receive data from the standard output, if you
  -- requested such a mailbox using 'Mailbox' for 'std_out'.  The
  -- output from the subprocess is read in arbitrarily sized chunks
  -- (currently this arbitrary size is 1024 bytes) and resulting
  -- chunks are placed in this 'Input'.

  , mbxStdErr :: Maybe FromProcess
  -- ^ Mailbox to receive data from the standard error, if you
  -- requested such a mailbox using 'Mailbox' for 'std_err'.  The
  -- output from the subprocess is read in arbitrarily sized chunks
  -- (currently this arbitrary size is 1024 bytes) and resulting
  -- chunks are placed in this 'Input'.

  , mbxHandle :: Process.ProcessHandle
  -- ^ A process handle.  The process begins running in the background
  -- immediately.  Remember that the subprocess will be terminated
  -- right away after you leave the 'ContT' monad.  To wait until the
  -- process finishes running, use 'Process.waitForProcess' from the
  -- "System.Process" module.
  }

convertCreateProcess :: CreateProcess -> Process.CreateProcess
convertCreateProcess a = Process.CreateProcess
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

-- | Create a 'CreateProcess' record with default settings.  The
-- default settings are:
--
-- * a raw command (as opposed to a shell command) is created
--
-- * the current working directory is not changed from the parent process
--
-- * the environment is not changed from the parent process
--
-- * standard input, standard output, and standard error are all
-- inherited from the parent process
--
-- * the parent's other file descriptors are also inherited
--
-- * a new process group is not created
--
-- * 'delegate_ctlc' is 'False'

proc
  :: String
  -- ^ The name of the program to run, such as @less@.
  -> [String]
  -- ^ Command-line arguments
  -> CreateProcess
proc prog args = CreateProcess
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


-- # Pipes

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


-- | A buffer that holds 10 messages.  I have no idea if this is the
-- ideal size.  Don't use an unbounded buffer, though, because with
-- unbounded producers an unbounded buffer will fill up your RAM.
messageBuffer :: Buffer a
messageBuffer = bounded 10

initialize
  :: (MonadSafe m, MonadIO m)
  => IO a
  -> (a -> IO ())
  -> m (a, ReleaseKey)
initialize make destroy = mask $ \_ -> do
  thing <- liftIO make
  key <- register (liftIO (destroy thing))
  return (thing, key)

-- | Creates a mailbox; seals it when done.
newMailbox
  :: (MonadSafe m, MonadIO m)
  => m (Output a, Input a, ReleaseKey)
newMailbox = liftM (\((inp, out, stm), key) -> (inp, out, key)) $
  initialize (spawn' messageBuffer)
  (\(_, _, seal) -> atomically seal)

background :: (MonadSafe m, MonadIO m) => IO a -> m (Async a, ReleaseKey)
background action= initialize (async action) cancel

processPump
  :: (MonadSafe m, MonadIO m)
  => Handle
  -> Input ByteString
  -> m ()
processPump handle input = do
  let pumper = runSafeT $ do
        register (liftIO $ ignoreIOExceptions $ hClose handle)
        liftIO . runEffect $
          fromInput input >-> consumeToHandle handle
  _ <- background pumper
  return ()

processPull
  :: (MonadSafe m, MonadIO m)
  => Handle
  -> Output ByteString
  -> m ()
processPull handle output = do
  let puller = runSafeT $ do
        register (liftIO $ ignoreIOExceptions $ hClose handle)
        liftIO . runEffect $
          produceFromHandle handle >-> toOutput output
  _ <- background puller
  return ()


makeToProcess
  :: (MonadSafe m, MonadIO m)
  => Handle
  -> m ToProcess
makeToProcess handle = do
  (out, inp, key) <- newMailbox
  processPump handle inp
  return $ ToProcess out key

makeFromProcess
  :: (MonadSafe m, MonadIO m)
  => Handle
  -> m FromProcess
makeFromProcess handle = do
  (out, inp, key) <- newMailbox
  processPull handle out
  return $ FromProcess inp key


makeBox
  :: Monad m
  => Maybe Handle
  -> (Handle -> m a)
  -> m (Maybe a)
makeBox mayHan mkr = case mayHan of
  Nothing -> return Nothing
  Just h -> liftM Just $ mkr h

makeBoxes
  :: (MonadIO m, MonadSafe m)
  => (Maybe Handle, Maybe Handle, Maybe Handle)
  -> m ( Maybe ToProcess, Maybe FromProcess, Maybe FromProcess )
makeBoxes (mayIn, mayOut, mayErr) = do
  inp <- makeBox mayIn makeToProcess
  out <- makeBox mayOut makeFromProcess
  err <- makeBox mayErr makeFromProcess
  return (inp, out, err)

{-
-- # Mailboxes

-- | Given a Handle that provides standard input to a process, return
-- an Output that can be used to put messages in for the standard
-- input.  Spawns another thread to feed to the process's standard
-- input.

        
-- | Given a Handle that provides standard output or standard error
-- from a process, return an Input that can be used to receive
-- messages.

-- # Subprocesses

-- Waiting on a process handle more than once will merely return the
-- same code more than once.  See the source code for System.Process.

useProcess
  :: CreateProcess
  -> ContT r IO ( Maybe Handle
                , Maybe Handle
                , Maybe Handle
                , Process.ProcessHandle)
useProcess ps = ContT (Control.Exception.bracket acq rel)
  where
    acq = Process.createProcess (convertCreateProcess ps)
    rel (i, o, e, h)= do
      Process.terminateProcess h
      _ <- Process.waitForProcess h
      let shut = maybe (return ()) (ignoreIOExceptions . hClose)
      shut i >> shut o >> shut e
      return ()

-- | Launch and use a subprocess.
--
-- /Warning/ - do not attempt to use any of the resources created by
-- the process after leaving the 'ContT' computation.  They are all
-- destroyed.  So any Pipes you create using the 'Output' or 'Input'
-- that connect to the process must finish their IO before you leave
-- the 'ContT' computation.  It's okay to return the
-- 'System.Exit.ExitCode' that you get from running the process, or
-- any data you get from the process--you just can't return something
-- that must perform IO to interact with the process.
--
-- Also, exiting the 'ContT' computation immediately destroys all
-- subprocesses.  If you want to make sure the process terminates
-- first, use 'Process.waitForProcess' on the handle which you can get
-- from 'mbxHandle' before leaving the 'ContT' computation.
--
-- The upside of this warning is that because all subprocess resources
-- are destroyed after leaving the 'ContT' computation, this function
-- is exception safe.
--
-- To increase the safety when using values with the 'ContT' type,
-- you can use 'safeCliff'.
createProcess
  :: CreateProcess
  -> ContT r IO Mailboxes
createProcess spec = do
  (inp, out, err, han) <- useProcess spec
  (inp', out', err') <- makeBoxes (inp, out, err)
  return $ Mailboxes inp' out' err' han

-- | Runs a thread in the background.  Be sure to use this every time
-- you run an 'Effect' that streams values to or from a mailbox
-- associated with a process, even if you only use a single
-- process. Otherwise, you might get deadlocks or errors such as
-- @thread blocked indefinitely in an STM transaction@.  The
-- 'Control.Concurrent.Async' that is returned allows you to later
-- kill the thread if you need to.  The associated thread will be
-- killed when the entire 'ContT' computation completes; to prevent
-- this from happening, use 'Control.Concurrent.Async.wait' from
-- "Control.Concurrent.Async".

background :: IO a -> ContT r IO (Async a)
background = ContT . Control.Concurrent.Async.withAsync

-- | Runs a 'ContT' computation.  Since the computation can only
-- return the unit type, this function increases safety by making it
-- difficult to inadvertently use values from the 'ContT' computation
-- outside of the computation.  That keeps you from using allocated
-- resources after they have been destroyed.
safeCliff :: ContT () IO ()-> IO ()
safeCliff = evalContT

{- $reexports
   * "Control.Concurrent.Async" reexports 'Async' and 'wait'

   * "Control.Monad.Trans.Cont" reexports the 'ContT' type constructor
      and data constructor, 'runContT', and 'evalContT'

   * "Pipes" reexports all bindings

   * "Pipes.Concurrent" reexports all bindings

   * "System.Exit" reexports all bindings

   * "System.Process" reexports 'waitForProcess'
-}
-}
