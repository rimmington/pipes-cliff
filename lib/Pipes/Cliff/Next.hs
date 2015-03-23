{-# LANGUAGE FlexibleContexts #-}
module Pipes.Cliff.Next where

import Control.Monad
import Control.Exception (IOException)
import System.IO
import qualified System.Process as Process
import System.Process (ProcessHandle)
import Pipes
import Pipes.Safe
import qualified Data.ByteString as BS
import qualified Pipes.Concurrent as PC
import Data.ByteString (ByteString)
import Control.Concurrent.MVar
import Control.Concurrent.Async
import Control.Monad.Trans.Reader

-- # Exception handling

handleException
  :: MonadIO m
  => Maybe HandleOopsie
  -> IOException
  -> ReaderT Env m ()
handleException mayOops exc = do
  spec <- asks envCmdSpec
  sender <- asks envErrorHandler
  let oops = Oopsie mayOops spec exc
  liftIO $ sender oops

-- | Run an action, taking all IO errors and sending them to the handler.
handleErrors
  :: (MonadCatch m, MonadIO m)
  => Maybe HandleOopsie
  -> m ()
  -> ReaderT Env m ()
handleErrors mayHandleOops act = catch act' catcher
  where
    act' = lift act
    catcher e = do
      spec <- asks envCmdSpec
      hndlr <- asks envErrorHandler
      let oops = Oopsie mayHandleOops spec e
      liftIO $ hndlr oops
      return ()

closeHandleNoThrow
  :: (MonadCatch m, MonadIO m)
  => Handle
  -> HandleDesc
  -> ReaderT Env m ()
closeHandleNoThrow hand desc = handleErrors (Just (HandleOopsie Closing desc))
  (liftIO $ hClose hand)

terminateProcess
  :: (MonadCatch m, MonadIO m)
  => Process.ProcessHandle
  -> ReaderT Env m ()
terminateProcess han = do
  _ <- handleErrors Nothing (liftIO (Process.terminateProcess han))
  handleErrors Nothing . liftIO $ do
    _ <- Process.waitForProcess han
    return ()


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

  , storeProcessHandle :: Maybe (MVar Process.ProcessHandle)
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
  , envProcessHandle :: Maybe (MVar ProcessHandle)
  , envCmdSpec :: CmdSpec
  }

envFromCreateProcess
  :: CreateProcess
  -> Env
envFromCreateProcess cp = Env
  { envErrorHandler = handler cp
  , envProcessHandle = storeProcessHandle cp
  , envCmdSpec = cmdspec cp
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
  , storeProcessHandle = Nothing
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

newMailbox'
  :: (MonadIO m, MonadSafe mi, MonadSafe mo)
  => m (Consumer a mi (), Producer a mo (), PC.STM ())
newMailbox' = liftM f (liftIO $ PC.spawn' messageBuffer)
  where
    f (PC.Output sndr, PC.Input rcvr, seal) = (csmr, prod, seal)
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

newMailboxToProcess
  :: (MonadSafe m, MonadSafe mo)
  => m (PC.Output a, Producer a mo (), PC.STM ())
newMailboxToProcess = bracket (liftIO $ PC.spawn' messageBuffer) rel use
  where
    rel (_, _, seal) = liftIO $ PC.atomically seal
    use (sndr, PC.Input rcvr, seal) = return (sndr, prod, seal)
      where
        prod = finally go (liftIO (PC.atomically seal))
          where
            go = do
              mayVal <- liftIO $ PC.atomically rcvr
              case mayVal of
                Nothing -> return ()
                Just val -> yield val >> go

newMailboxFromProcess
  :: (MonadSafe m, MonadSafe mi)
  => m (Consumer a mi (), PC.Input a, PC.STM ())
newMailboxFromProcess = bracket (liftIO $ PC.spawn' messageBuffer) rel use
  where
    rel (_, _, seal) = liftIO $ PC.atomically seal
    use (PC.Output sndr, rcvr, seal) = return (csmr, rcvr, seal)
      where
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
produceFromHandle hDesc h = fmap f ask
  where
    f ev = catch produce hndlr
      where
        closeHan = runReaderT (closeHandleNoThrow h hDesc) ev
        produce = liftIO (BS.hGetSome h bufSize) >>= go
        go bs
            | BS.null bs = closeHan
            | otherwise = yield bs >> produce
        hndlr e = do
          closeHan
          lift (runReaderT (handleException oops e) ev)
    oops = Just (HandleOopsie Reading hDesc)


acquire
  :: MonadSafe m
  => Base m a
  -> (a -> Base m ())
  -> m a
acquire acq rel = do
  a <- mask_ $ do
    a' <- liftBase acq
    _ <- register (rel a')
    return a'
  return a


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
consumeToHandle h = fmap f ask
  where
    f ev = catch consume hndlr
      where
        closeHan = runReaderT (closeHandleNoThrow h Input) ev
        consume = do
          bs <- await
          liftIO $ BS.hPut h bs
          consume
        hndlr e = do
          closeHan
          lift (runReaderT (handleException oops e) ev)
    oops = Just (HandleOopsie Writing Input)

-- | Creates a background thread that will consume to the given Handle
-- from the given Producer.  Takes ownership of the 'Handle' and
-- closes it when done.
backgroundSendToProcess
  :: MonadSafe m
  => Handle
  -> Producer ByteString (SafeT IO) ()
  -> ReaderT Env m ()
backgroundSendToProcess han prod = do
  ev <- ask
  let csmr = runReader (consumeToHandle han) ev
      act = runSafeT . runEffect $ prod >-> csmr
  _ <- lift $ background act
  return ()

-- | Creates a background thread that will produce from the given
-- Handle into the given Consumer.  Takes possession of the Handle and
-- closes it when done.
backgroundReceiveFromProcess
  :: MonadSafe m
  => HandleDesc
  -> Handle
  -> Consumer ByteString (SafeT IO) ()
  -> ReaderT Env m ()
backgroundReceiveFromProcess desc han csmr = do
  ev <- ask
  let prod = runReader (produceFromHandle desc han) ev
      act = runSafeT . runEffect $ prod >-> csmr
  _ <- lift $ background act
  return ()


destroyProcess
  :: (MonadCatch m, MonadIO m)
  => (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
  -> ReaderT Env m ()
destroyProcess (inp, out, err, han) = do
  let close mayH d = case mayH of
        Nothing -> return ()
        Just h -> closeHandleNoThrow h d
  close inp Input
  close out Output
  close err Error
  terminateProcess han

-- | Creates a subprocess.  Registers destroyers for each handle
-- created, as well as for the ProcessHandle.
createProcess
  :: (MonadSafe m, MonadCatch (Base m))
  => Process.CreateProcess
  -> ReaderT Env m (Maybe Handle, Maybe Handle, Maybe Handle)
createProcess cp = mask $ \restore -> do
  (mayIn, mayOut, mayErr, han) <- liftIO $ Process.createProcess cp
  ev <- ask
  let close mayHan desc = maybe (return ())
        (\h -> runReaderT (closeHandleNoThrow h desc) ev) mayHan
  _ <- register (close mayIn Input)
  _ <- register (close mayOut Output)
  _ <- register (close mayErr Error)
  _ <- register (runReaderT (terminateProcess han) ev)
  mayHanVar <- asks envProcessHandle
  case mayHanVar of
    Nothing -> return ()
    Just hanVar -> liftIO $ putMVar hanVar han
  restore $ return (mayIn, mayOut, mayErr)

runCreateProcess
  :: (MonadSafe m, MonadCatch (Base m))
  => Maybe NonPipe
  -> Maybe NonPipe
  -> Maybe NonPipe
  -> CreateProcess
  -> m (Maybe Handle, Maybe Handle, Maybe Handle, Env)
runCreateProcess inp out err cp = do
  let ev = envFromCreateProcess cp
  (inp', out', err') <- runReaderT
      (createProcess (convertCreateProcess inp out err cp)) ev
  return (inp', out', err', ev)

-- | Runs in the background an effect, typically one that is moving
-- data from one process to another.  For examples of its usage, see
-- "Pipes.Cliff.Examples".  The associated thread is killed when the
-- 'SafeT' computation completes.
conveyor :: Effect (Pipes.Safe.SafeT IO) () -> Pipes.Safe.SafeT IO ()
conveyor efct
  = (background . liftIO . runSafeT . runEffect $ efct) >> return ()

-- | A version of 'Control.Concurrent.Async.wait' with an overloaded
-- 'MonadIO' return type.  Allows you to wait for the return value of
-- threads launched with 'background'.  If the thread throws an
-- exception, 'waitForThread' will throw that same exception.
waitForThread :: MonadIO m => Async a -> m a
waitForThread = liftIO . wait

-- | Creating Proxy

pipeNone
  :: (MonadSafe m, MonadCatch (Base m))
  => NonPipe
  -> NonPipe
  -> NonPipe
  -> CreateProcess
  -> m ()
pipeNone inp out err cp = flip runReaderT (envFromCreateProcess cp) $ do
  _ <- createProcess
    (convertCreateProcess (Just inp) (Just out) (Just err) cp)
  return ()

processPump
  :: (MonadCatch m, MonadIO m)
  => (a -> PC.STM Bool)
  -- ^ Reception mailbox
  -> PC.STM ()
  -- ^ Sealer
  -> Consumer a m ()
processPump toStdinMbox seal = go
  where
    go = do
      bs <- await
      res <- liftIO . PC.atomically . toStdinMbox $ bs
      if res then go else liftIO (PC.atomically seal)



pipeInput
  :: (MonadSafe m, MonadCatch (Base m))
  => NonPipe
  -- ^ Standard output
  -> NonPipe
  -- ^ Standard error
  -> CreateProcess
  -> Consumer ByteString m ()
pipeInput out err cp = do
  (Just inp, _, _, ev) <- runCreateProcess Nothing (Just out) (Just err) cp
  (PC.Output toStdinMbox, fromStdinMbox, seal) <- newMailboxToProcess
  runReaderT (backgroundSendToProcess inp fromStdinMbox) ev
  processPump toStdinMbox seal

processPuller
  :: (MonadCatch m, MonadIO m)
  => PC.STM (Maybe a)
  -- ^ Where the process delivers new ByteStrings
  -> PC.STM ()
  -- ^ Seals the mailbox
  -> Producer a m ()
processPuller fromOutMbox seal = go
  where
    go = do
      mayBs <- liftIO $ PC.atomically fromOutMbox
      case mayBs of
        Just bs -> yield bs >> go
        Nothing -> liftIO (PC.atomically seal)


runInputHandle
  :: MonadSafe mi
  => Handle
  -> Env
  -> Consumer ByteString mi ()
runInputHandle inp ev = do
  (PC.Output toStdinMbox, fromStdinMbox, seal) <- newMailboxToProcess
  runReaderT (backgroundSendToProcess inp fromStdinMbox) ev
  processPump toStdinMbox seal

runOutputHandle
  :: MonadSafe mo
  => HandleDesc
  -> Handle
  -> Env
  -> Producer ByteString mo ()
runOutputHandle desc out ev = do
  (toStdoutMbox, PC.Input fromStdoutMbox, seal) <- newMailboxFromProcess
  runReaderT (backgroundReceiveFromProcess desc out toStdoutMbox) ev
  processPuller fromStdoutMbox seal

pipeOutput
  :: (MonadSafe m, MonadCatch (Base m))
  => NonPipe
  -- ^ Standard input
  -> NonPipe
  -- ^ Standard error
  -> CreateProcess
  -> Producer ByteString m ()
pipeOutput inp err cp = do
  (_, Just out, _, ev) <- runCreateProcess (Just inp) Nothing (Just err) cp
  runOutputHandle Output out ev

pipeError
  :: (MonadSafe m, MonadCatch (Base m))
  => NonPipe
  -- ^ Standard input
  -> NonPipe
  -- ^ Standard output
  -> CreateProcess
  -> Producer ByteString m ()
pipeError inp out cp = do
  (_, _, Just err, ev) <- runCreateProcess (Just inp) (Just out) Nothing cp
  runOutputHandle Error err ev

pipeInputOutput
  :: (MonadSafe mi, MonadSafe mo, MonadSafe m, MonadCatch (Base m))
  => NonPipe
  -- ^ Standard error
  -> CreateProcess
  -> m (Consumer ByteString mi (), Producer ByteString mo ())
pipeInputOutput err cp = do
  (Just inp, Just out, _, ev) <-
    runCreateProcess Nothing Nothing (Just err) cp
  return (runInputHandle inp ev, runOutputHandle Output out ev)

{-
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
