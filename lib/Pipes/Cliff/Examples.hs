-- | Examples using "Pipes.Cliff".  You will want to look at the
-- source code itself; viewing just the Haddocks will not help you
-- much.  You can view the source using Haddock if you used
-- @--hyperlink-source@ when building the library or if you are
-- viewing this on Hackage; look for the @Source@ link.  Or, you can
-- find the source at
--
-- <https://github.com/massysett/pipes-cliff/blob/master/lib/Pipes/Cliff/Examples.hs>
--
-- __Be sure to use the @-threaded@ option__ when compiling code that
-- uses "Pipes.Cliff", including this code; see the warning in
-- "Pipes.Cliff" for more details.

module Pipes.Cliff.Examples where

import Pipes.Cliff
import qualified Data.ByteString.Char8 as BS8

-- | Streams an infinite list of numbers to @less@.
numsToLess :: IO ExitCode
numsToLess = evalContT $ do
  Mailboxes (Just out) _ _ han <- createProcess
    (proc "less" []) { std_in = Mailbox }
  lift . runEffect $ produceNumbers >-> toOutput out
  lift $ waitForProcess han

-- | Streams an infinite list of numbers to @tr@ and then to @less@.
-- Perfectly useless, but shows how to build pipelines.  Notice how
-- the components of the pipeline are run in the 'background'; if you
-- run one of them in the foreground, you might get a deadlock.
alphaNumbers :: IO ExitCode
alphaNumbers = evalContT $ do
  Mailboxes (Just toTr) (Just fromTr) _ _ <- createProcess
    (proc "tr" ["[0-9]", "[a-z]"]) { std_in = Mailbox
                                         , std_out = Mailbox
                                         }
  Mailboxes (Just toLess) _ _ lessHan <- createProcess
    (proc "less" []) { std_in = Mailbox }
  _ <- background . runEffect $ fromInput fromTr >-> toOutput toLess
  _ <- background . runEffect $ produceNumbers >-> toOutput toTr
  r <- lift $ waitForProcess lessHan
  return r

-- | Produces a stream of ByteString, where each ByteString is a shown
-- integer.

produceNumbers :: Monad m => Producer BS8.ByteString m ()
produceNumbers = each . fmap mkNumStr $ [(0 :: Int) ..]
  where
    mkNumStr = flip BS8.snoc '\n' . BS8.pack . show
