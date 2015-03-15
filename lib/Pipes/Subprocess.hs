{-# LANGUAGE RankNTypes #-}
module Pipes.Subprocess where

import Control.Monad.IO.Class ()
import System.Exit (ExitCode)
import Pipes
  ( MonadIO, Producer', liftIO, yield, Consumer',
    await, runEffect, (>->), lift, Proxy, X)
import Pipes.Safe (SafeT)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified System.Process as Process
import System.IO (Handle, hClose)
import qualified Control.Exception
import Control.Monad.Trans.Cont (ContT(ContT), evalContT)
import Control.Concurrent.Async (wait, Async, withAsync)

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


-- | I have no idea what this should be
bufSize :: Int
bufSize = 1024

produceFromHandle :: MonadIO m => Handle -> Producer' ByteString m ()
produceFromHandle h = liftIO (BS.hGetSome h bufSize) >>= go
  where
    go bs | BS.null bs = return ()
          | otherwise = yield bs >> produceFromHandle h

consumeToHandle :: MonadIO m => Handle -> Consumer' ByteString m r
consumeToHandle h = do
  bs <- await
  liftIO $ BS.hPut h bs
  consumeToHandle h

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

runProcess
  :: (MonadIO xm, MonadIO ym, MonadIO zm)
  => RunProducer' xm xr xc
  -- ^ Stdin
  -> RunConsumer' ym () yc
  -- ^ Stdout
  -> RunConsumer' zm () zc
  -- ^ Stderr
  -> ProcSpec
  -> IO (ExitCode, xc, yc, zc)
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

data ProcSpec = ProcSpec
  { program :: String
  , args :: [String]
  , cwd :: Maybe FilePath
  , env :: Maybe [(String, String)]
  , close_fds :: Bool
  , create_group :: Bool
  , delegate_ctlc :: Bool
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

spawn :: IO a -> ContT b IO (Async a)
spawn io = ContT (withAsync io)

data RunProxy a' a b' b m r c
  = RunProxy (Proxy a' a b' b m r) (m r -> IO c)

type RunProducer' m r c = forall x' x. RunProxy x' x () ByteString m r c
type RunConsumer' m r c = forall y' y. RunProxy () ByteString y' y m r c
type RunProducer m = RunProxy X () () ByteString m () ()
type RunConsumer m = RunProxy () ByteString () X m () ()

fromStdin :: RunProducer IO
fromStdin = undefined

toStdout :: RunConsumer IO
toStdout = undefined

toStderr :: RunConsumer IO
toStderr = undefined

fromFile :: FilePath -> IO (RunProducer (SafeT IO))
fromFile = undefined

toFile :: FilePath -> IO (RunConsumer (SafeT IO))
toFile = undefined

