{-# LANGUAGE RankNTypes #-}
-- | Spawn subprocesses and interact with them using "Pipes"
--
-- The interface in this module deliberately resembles the interface
-- in "System.Process".  However, one consequence of this is that you
-- will not want to have unqualified names from this module and from
-- "System.Process" in scope at the same time.
--
-- As in "System.Process", you create a subprocess by creating a
-- 'CreateProcess' record and then applying a function to that
-- record.  Unlike "System.Process", you use functions such as
-- 'pipeInput' or 'pipeInputOutput' to specify what streams you want
-- to use a 'Proxy' for and what streams you wish to be 'Inherit'ed
-- or if you want to 'UseHandle'.  You then send or receive
-- information using the returned 'Proxy'.
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
-- This module relies on the "Pipes", "Pipes.Safe", and
-- "System.Process" modules.  You will want to have basic
-- familiarity with what all of those modules do before using this
-- module.  It uses "Control.Concurrent.Async" and
-- "Pipes.Concurrent" behind the scenes; you don't need to know how
-- these work unless you're curious.
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
module Pipes.Cliff
  ( -- * Specifying a subprocess's properties
    NonPipe(..)
  , CreateProcess(..)
  , procSpec

  -- * Creating processes
  -- $process
  , pipeNone
  , pipeInput
  , pipeOutput
  , pipeError
  , pipeInputOutput
  , pipeInputError
  , pipeOutputError
  , pipeInputOutputError

  -- * Conveniences

  -- | These make it a bit easier to write both simple and
  -- complicated pipelines.

  , conveyor
  , waitForProcess

  -- * Re-exports
  -- $reexports
  , module Pipes
  , module Pipes.Safe
  , module System.Exit
  , module System.Process
  ) where

import System.IO
import Pipes
import Pipes.Concurrent
import Control.Concurrent.Async (Async, async, cancel)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified System.Process as Process
import System.Process (ProcessHandle)
import qualified Control.Exception
import Pipes.Safe (runSafeT)
import qualified Pipes.Safe
import System.Exit
import System.Environment (getProgName)
import qualified System.IO as IO
import Control.Monad (when)

-- # Exception handling

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
  :: Bool
  -- ^ Be quiet?
  -> Handle
  -> IO ()
closeHandle q h = ignoreIOExceptions q "closing handle" (hClose h)

-- | Terminate a process, ignoring all IO exceptions.
terminateProcess
  :: Bool
  -- ^ Be quiet?
  -> Process.ProcessHandle
  -> IO ()
terminateProcess q h = ignoreIOExceptions q "terminating process" $ do
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
-- * standard input, standard output, and standard error are all
-- inherited from the parent process
--
-- * the parent's other file descriptors are also inherited
--
-- * a new process group is not created
--
-- * 'delegate_ctlc' is 'False'
--
-- * 'quiet' is 'False'

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
  }


-- # Pipes

-- | I have no idea what this should be.  I'll start with a simple
-- small value and see how it works.
bufSize :: Int
bufSize = 1024

-- | Create a 'Producer' from a 'Handle'.  The 'Producer' will get
-- 'ByteString' from the 'Handle' and produce them.  Does nothing to
-- close the given 'Handle' at any time.
--
-- TODO IO Errors
produceFromHandle
  :: MonadIO m
  => Handle
  -> Producer ByteString m ()
produceFromHandle h = liftIO (BS.hGetSome h bufSize) >>= go
  where
    go bs
      | BS.null bs = return ()
      | otherwise = yield bs >> produceFromHandle h


-- | Create a 'Consumer' from a 'Handle'.  The 'Consumer' will put
-- each 'ByteString' it receives into the 'Handle'.  Does nothing to
-- close the handle at any time.
--
-- TODO IO errors
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

-- | Acquires a resource and registers a finalizer.
initialize
  :: (Pipes.Safe.MonadSafe m, MonadIO m)
  => IO a
  -> (a -> IO ())
  -> m a
initialize make destroy = Pipes.Safe.mask $ \_ -> do
  thing <- liftIO make
  _ <- Pipes.Safe.register (liftIO (destroy thing))
  return thing

-- | Creates a mailbox; seals it when done.
newMailbox
  :: (Pipes.Safe.MonadSafe m, MonadIO m)
  => m (Output a, Input a, STM ())
newMailbox =
  initialize (spawn' messageBuffer)
  (\(_, _, seal) -> atomically seal)

-- | Runs a thread in the background.  Initializes a finalizer that
-- will cancel the thread.
background :: (Pipes.Safe.MonadSafe m, MonadIO m) => IO a -> m (Async a)
background action = initialize (async action) cancel

-- | Creates a thread that will run in the background and pump
-- messages from the given mailbox to the process via its handle.
-- Closes the Handle when done.
processPump
  :: (MonadIO m, Pipes.Safe.MonadSafe m)
  => Bool
  -- ^ Quiet?
  -> Handle
  -> (Input ByteString, STM ())
  -> m ()
processPump beQuiet hndle (input, seal) = do
  let pumper = flip Control.Exception.finally cleanup .
        runEffect $
          fromInput input >-> consumeToHandle hndle
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
  :: (Pipes.Safe.MonadSafe m, MonadIO m)
  => Bool
  -- ^ Quiet?
  -> Handle
  -> (Output ByteString, STM ())
  -- ^ Output box, paired with action to close the box.
  -> m ()
processPull beQuiet hndle (output, seal) = do
  let puller = flip Control.Exception.finally cleanup .
        runEffect $
          produceFromHandle hndle >-> toOutput output
  _ <- background puller
  return ()
  where
    cleanup = liftIO $ do
      closeHandle beQuiet hndle
      atomically seal


-- | Creates a mailbox that sends messages to the given process, and
-- sets up and runs threads to pump messages to the process.
makeToProcess
  :: (Pipes.Safe.MonadSafe mp, MonadIO mp, Pipes.Safe.MonadSafe m, MonadIO m)
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
  :: (Pipes.Safe.MonadSafe m, MonadIO m, Pipes.Safe.MonadSafe mp, MonadIO mp)
  => Bool
  -- ^ Quiet?
  -> Handle
  -> m (Producer ByteString mp ())
makeFromProcess beQuiet hndle = do
  (out, inp, seal) <- newMailbox
  processPull beQuiet hndle (out, seal)
  return $ fromInput inp `Pipes.Safe.finally` (liftIO (atomically seal))


-- # Subprocesses

-- Waiting on a process handle more than once will merely return the
-- same code more than once.  See the source code for System.Process.

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


-- | Creates a subprocess.  Registers destroyers for each handle
-- created, as well as for the ProcessHandle.
createProcess
  :: (MonadIO m, Pipes.Safe.MonadSafe m)
  => Bool
  -- ^ Quiet?
  -> Process.CreateProcess
  -> m (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
createProcess beQuiet cp = initialize (Process.createProcess cp) destroy
  where
    destroy (mayIn, mayOut, mayErr, han) = do
      let close = maybe (return ()) (closeHandle beQuiet)
      close mayIn
      close mayOut
      close mayErr
      terminateProcess beQuiet han


-- | Runs in the background an effect, typically one that is moving
-- data from one process to another.  For examples of its usage, see
-- "Pipes.Cliff.Examples".  The associated thread is killed when the
-- 'SafeT' computation completes.
conveyor :: Effect (Pipes.Safe.SafeT IO) a -> Pipes.Safe.SafeT IO ()
conveyor efct
  = (background . liftIO . runSafeT . runEffect $ efct) >> return ()

-- | A version of 'System.Process.waitForProcess' with an overloaded
-- 'MonadIO' return type.
waitForProcess :: MonadIO m => ProcessHandle -> m ExitCode
waitForProcess h = liftIO $ Process.waitForProcess h

{- $process

Each of these functions creates a process.  The process begins
running immediately in a separate process while your Haskell program
continues concurrently.  A function is provided for each possible
combination of standard input, standard output, and standard error.
Use the 'NonPipe' type to describe what you want to do with streams
you do NOT want to create a stream for.  For example, to create a
subprocess that does not create a Pipe for any of the standard
streams, use 'pipeNone'.  You must describe what you want done with
standard input, standard output, and standard error.  To create a
subprocess that creates a Pipe for standard input and standard
output, use 'pipeInputOutput'.  You must describe what you want done
with standard error.  A 'Producer' is returned for standard output
and a 'Consumer' for standard input.

Do NOT attempt to use any of the resources created by this function
outside of the 'Pipes.Safe.MonadSafe' computation.  The
'Pipes.Safe.MonadSafe' computation will destroy all resources when
the 'Pipes.Safe.MonadSafe' computation is complete.  This means that
all these functions are exception safe: all resources--threads and
processes included--are destroyed when the 'Pipes.Safe.MonadSafe'
computation is complete, even if an exception is thrown.  However,
this does mean that you need to stay in the 'Pipes.Safe.MonadSafe'
computation if you need to keep a resource around.  'waitForProcess'
can be handy for this.  All functions in this section return a
'ProcessHandle' for use with 'waitForProcess'.

Each 'Proxy' automatically destroys associated file handles and
other behind-the-scenes resources after its computation finishes
running; that's why the monad stack of each 'Proxy' must contain a
'Pipes.Safe.MonadSafe'.  For an example of how to use this, consule
"Pipes.Cliff.Examples".

-}
pipeNone
  :: (MonadIO m, Pipes.Safe.MonadSafe m)
  => NonPipe
  -- ^ Standard input
  -> NonPipe
  -- ^ Standard output
  -> NonPipe
  -- ^ Standard error
  -> CreateProcess
  -> m Process.ProcessHandle
pipeNone sIn sOut sErr cp = do
  (_, _, _, han) <- createProcess (quiet cp) cp'
  return han
  where
    cp' = convertCreateProcess (Just sIn) (Just sOut) (Just sErr)
      cp

pipeInput
  :: (MonadIO mi, Pipes.Safe.MonadSafe mi, MonadIO m, Pipes.Safe.MonadSafe m)
  => NonPipe
  -- ^ Standard output
  -> NonPipe
  -- ^ Standard error
  -> CreateProcess
  -> m (Consumer ByteString mi (), Process.ProcessHandle)
  -- ^ A 'Consumer' for standard input, and the 'ProcessHandle'
pipeInput sOut sErr cp = do
  (Just inp, _, _, han) <- createProcess (quiet cp) cp'
  inp' <- makeToProcess (quiet cp) inp
  return (inp', han)
  where
    cp' = convertCreateProcess Nothing (Just sOut) (Just sErr)
      cp

pipeOutput
  :: (MonadIO mi, Pipes.Safe.MonadSafe mi, MonadIO m, Pipes.Safe.MonadSafe m)
  => NonPipe
  -- ^ Standard input
  -> NonPipe
  -- ^ Standard error
  -> CreateProcess
  -> m (Producer ByteString mi (), Process.ProcessHandle)
  -- ^ A 'Producer' for standard output, and the 'ProcessHandle'
pipeOutput sIn sErr cp = do
  (_, Just out, _, han) <- createProcess (quiet cp) cp'
  out' <- makeFromProcess (quiet cp) out
  return (out', han)
  where
    cp' = convertCreateProcess (Just sIn) Nothing (Just sErr) cp

pipeError
  :: (MonadIO mi, Pipes.Safe.MonadSafe mi, MonadIO m, Pipes.Safe.MonadSafe m)
  => NonPipe
  -- ^ Standard input
  -> NonPipe
  -- ^ Standard output
  -> CreateProcess
  -> m (Producer ByteString mi (), Process.ProcessHandle)
  -- ^ A 'Producer' for standard error, and the 'ProcessHandle'
pipeError sIn sOut cp = do
  (_, _, Just err, han) <- createProcess (quiet cp) cp'
  err' <- makeFromProcess (quiet cp) err
  return (err', han)
  where
    cp' = convertCreateProcess (Just sIn) (Just sOut) Nothing cp

pipeInputOutput
  :: ( MonadIO mi, Pipes.Safe.MonadSafe mi, MonadIO mo, Pipes.Safe.MonadSafe mo
     , MonadIO m, Pipes.Safe.MonadSafe m)
  => NonPipe
  -- ^ Standard error
  -> CreateProcess
  -> m ( Consumer ByteString mi ()
       , Producer ByteString mo ()
       , Process.ProcessHandle
       )
  -- ^ A 'Consumer' for standard input, a 'Producer' for standard
  -- output, and a 'ProcessHandle'
pipeInputOutput sErr cp = do
  (Just inp, Just out, _, han) <- createProcess (quiet cp) cp'
  inp' <- makeToProcess (quiet cp) inp
  out' <- makeFromProcess (quiet cp) out
  return (inp', out', han)
  where
    cp' = convertCreateProcess Nothing Nothing (Just sErr) cp

pipeInputError
  :: ( MonadIO mi, Pipes.Safe.MonadSafe mi, MonadIO mo, Pipes.Safe.MonadSafe mo
     , MonadIO m, Pipes.Safe.MonadSafe m)
  => NonPipe
  -- ^ Standard output
  -> CreateProcess
  -> m ( Consumer ByteString mi ()
       , Producer ByteString mo ()
       , Process.ProcessHandle
       )
  -- ^ A 'Consumer' for standard input, a 'Producer' for standard
  -- error, and a 'ProcessHandle'
pipeInputError sOut cp = do
  (Just inp, _, Just err, han) <- createProcess (quiet cp) cp'
  inp' <- makeToProcess (quiet cp) inp
  err' <- makeFromProcess (quiet cp) err
  return (inp', err', han)
  where
    cp' = convertCreateProcess Nothing (Just sOut) Nothing cp

pipeOutputError
  :: ( MonadIO mi, Pipes.Safe.MonadSafe mi, MonadIO mo
     , Pipes.Safe.MonadSafe mo, MonadIO m
     , Pipes.Safe.MonadSafe m)
  => NonPipe
  -- ^ Standard input
  -> CreateProcess
  -> m ( Producer ByteString mi ()
       , Producer ByteString mo ()
       , Process.ProcessHandle
       )
  -- ^ A 'Producer' for standard output, a 'Producer' for standard
  -- error, and a 'ProcessHandle'
pipeOutputError sIn cp = do
  (_, Just out, Just err, han) <- createProcess (quiet cp) cp'
  out' <- makeFromProcess (quiet cp) out
  err' <- makeFromProcess (quiet cp) err
  return (out', err', han)
  where
    cp' = convertCreateProcess (Just sIn) Nothing Nothing cp

pipeInputOutputError
  :: ( MonadIO mi, Pipes.Safe.MonadSafe mi, MonadIO mo, Pipes.Safe.MonadSafe mo,
       MonadIO me, Pipes.Safe.MonadSafe me, MonadIO m, Pipes.Safe.MonadSafe m)
  => CreateProcess
  -> m ( Consumer ByteString mi ()
       , Producer ByteString mo ()
       , Producer ByteString me ()
       , Process.ProcessHandle
       )
  -- ^ A 'Consumer' for standard input, a 'Producer' for standard
  -- output, a 'Producer' for standard error, and a 'ProcessHandle'
pipeInputOutputError cp = do
  (Just inp, Just out, Just err, han) <- createProcess (quiet cp) cp'
  inp' <- makeToProcess (quiet cp) inp
  out' <- makeFromProcess (quiet cp) out
  err' <- makeFromProcess (quiet cp) err
  return (inp', out', err', han)
  where
    cp' = convertCreateProcess Nothing Nothing Nothing cp

{- $reexports

   * "Pipes" reexports all bindings

   * "Pipes.Safe" reexports 'runSafeT'

   * "System.Exit" reexports all bindings

   * "System.Process" reexports 'ProcessHandle'

-}
