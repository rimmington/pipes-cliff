-- | Examples using "Pipes.Proctee".  You will want to look at the
-- source code itself; viewing just the Haddocks will not help you
-- much.  You can view the source using Haddock if you used
-- @--hyperlink-source@ when building the library or if you are
-- viewing this on Hackage; look for the @Source@ link.  Or, you can
-- find the source at
--
-- <https://github.com/massysett/pipes-process/blob/master/lib/Pipes/Proctee/Examples.hs>

module Pipes.Proctee.Examples where

import Pipes
import Pipes.Proctee
import Pipes.Concurrent
import qualified Pipes.Proctee.Mailboxes as M
import Control.Monad.Trans.Cont
import qualified Data.ByteString.Char8 as BS8
import System.Exit
import System.Process (waitForProcess)

-- | Streams an infinite list of numbers to @less@.
numsToLess :: IO ExitCode
numsToLess = runProcess (useProxy produceNumbers id) inherit inherit
  (procSpec "less" [])

numsToLess' :: IO ExitCode
numsToLess' = flip runContT return $ do
  M.Mailboxes (Just out) _ _ han <- M.createProcess
    (M.procSpec "less" []) { M.std_in = M.Mailbox }
  lift . runEffect $ produceNumbers >-> toOutput out
  lift $ waitForProcess han

-- | Streams an infinite list of numbers to @tr@ and then to @less@.
-- Perfectly useless, but shows how to build pipelines.
alphaNumbers :: IO ExitCode
alphaNumbers = flip runContT return $ do
  M.Mailboxes (Just toTr) (Just fromTr) _ _ <- M.createProcess
    (M.procSpec "tr" ["[0-9]", "[a-z]"]) { M.std_in = M.Mailbox
                                         , M.std_out = M.Mailbox
                                         }
  M.Mailboxes (Just toLess) _ _ lessHan <- M.createProcess
    (M.procSpec "less" []) { M.std_in = M.Mailbox }
  _ <- M.background . runEffect $ fromInput fromTr >-> toOutput toLess
  _ <- M.background . runEffect $ produceNumbers >-> toOutput toTr
  r <- lift $ waitForProcess lessHan
  return r

-- | Produces a stream of ByteString, where each ByteString is a shown
-- integer.

produceNumbers :: Monad m => Producer BS8.ByteString m ()
produceNumbers = each . fmap mkNumStr $ [(0 :: Int) ..]
  where
    mkNumStr = flip BS8.snoc '\n' . BS8.pack . show
