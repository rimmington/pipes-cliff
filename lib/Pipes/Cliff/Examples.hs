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
import qualified Pipes.Prelude as P
import qualified Data.ByteString.Char8 as BS8

-- | Streams an infinite list of numbers to @less@.
-- The 'Effect' that streams values to the process is run in the
-- background by using 'conveyor', even though there is only one
-- subprocess.  This is typically what you want.

numsToLess :: IO ExitCode
numsToLess = runSafeT $ do
  (toLess, han) <- pipeInput Inherit Inherit (procSpec "less" [])
  conveyor $ produceNumbers >-> toLess
  waitForProcess han

-- | Streams an infinite list of numbers to @tr@ and then to @less@.
-- Perfectly useless, but shows how to build pipelines.  Notice how
-- the components of the pipeline are run in the background using
-- 'conveyor'; if you run one of them in the foreground, you might get
-- a deadlock.

alphaNumbers :: IO ExitCode
alphaNumbers = runSafeT $ do
  (toTr, fromTr, _) <- pipeInputOutput Inherit
    (procSpec "tr" ["[0-9]", "[a-z]"])
  (toLess, lessHan) <- pipeInput Inherit Inherit
    (procSpec "less" [])
  conveyor $ produceNumbers >-> toTr
  conveyor $ fromTr >-> toLess
  waitForProcess lessHan


-- | Produces a stream of ByteString, where each ByteString is a shown
-- integer.

produceNumbers :: Monad m => Producer BS8.ByteString m ()
produceNumbers = each . fmap mkNumStr $ [(0 :: Int) ..]
  where
    mkNumStr = flip BS8.snoc '\n' . BS8.pack . show


-- | Like 'alphaNumbers' but just sends a limited number
-- of numbers to @cat@.  A useful test to make sure that pipelines
-- shut down automatically.
limitedAlphaNumbers :: IO ExitCode
limitedAlphaNumbers = runSafeT $ do
  (toTr, fromTr, _) <- pipeInputOutput Inherit
    (procSpec "tr" ["[0-9]", "[a-z]"])
  (toCat, catHan) <- pipeInput Inherit Inherit
    (procSpec "cat" [])
  conveyor $ produceNumbers >-> P.take 300 >-> toTr
  conveyor $ fromTr >-> toCat
  waitForProcess catHan

-- | Produces a finite list of numbers, sends it to @tr@ for some
-- mangling, and then puts the results into a 'BS8.ByteString' for
-- further processing.  Unlike previous examples, there is no use of
-- 'waitForProcess'.  This is OK because the 'Effect' that retrieves
-- the results from @tr@ will pull all the data; the @tr@ process will
-- then shut down because its standard input will be closed when the
-- source 'Producer' is exhausted.  This example shows how you can use
-- this library to place the results of a pipeline into a simple
-- strict data type.
--
-- When the 'SafeT' computation completes, a
-- 'System.Process.terminateProcess' is automatically sent to the @tr@
-- process--which does nothing, as @tr@ has already died.  Only after
-- the process is waited for is it fully removed from the system
-- process table.  A 'System.Process.waitForProcess' from
-- "System.Process" is automatically done as well.  Therefore, you
-- will not get zombie processes if you use this library.
alphaNumbersByteString :: IO BS8.ByteString
alphaNumbersByteString = runSafeT $ do
  (toTr, fromTr, _) <- pipeInputOutput Inherit
    (procSpec "tr" ["[0-9]", "[a-z]"])
  conveyor $ produceNumbers >-> P.take 300 >-> toTr
  P.fold BS8.append BS8.empty id fromTr
