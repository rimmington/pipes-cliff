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
-- background, even though there is only one subprocess.  If you did
-- not run the process in the background, then here's what would
-- happen:
--
-- 1.  'createProcess' launches @less@.
--
-- 2.  The line with 'runEffect' runs in the main Haskell thread.  It
-- streams @ByteString@s to @less@.  You page through @less@ as usual.
-- Just don't press @G@ to go to the end of the file, as there is no
-- end--it's an infinite list of numbers.  Your memory is not filled
-- with an infinite list, though.  The mailbox leading in to @less@ is
-- limited in size, so the 'Producer' that is making the numbers
-- blocks until space is available in the mailbox.
--
-- 3.  You quit @less@ with @q@.  But the IO action with 'runEffect'
-- is still running.  It doesn't fill up your memory--the mailbox
-- leading to @less@ is limited in size.  So the computation just
-- blocks.  Your console shows no activity.
--
-- 4.  If you're lucky, the runtime says
-- @thread blocked indefinitely in an STM transaction@
-- and quits.
-- If you're unlucky, you just get a console with no activity.
-- You hit control-C.
--
-- 5.  The IO action with 'waitForProcess' never runs.
--
-- By using 'background', here's what happens:
--
-- 1.  'createProcess' launches @less@.
--
-- 2.  The line with 'runEffect' runs in another Haskell thread.  It
-- streams @ByteString@ to @less@.  You page through @less@ as usual.
--
-- 3.  Immediately after @less@ opens, the main Haskell thread
-- proceeds to the IO action with 'waitForProcess'.  The main thread
-- pauses to wait until @less@ exits.
--
-- 4.  The 'ContT' computation returns the exit code from the @less@
-- process.
--
-- 5.  The 'ContT' computation destroys all resources created in the
-- 'ContT' computation.  The @less@ process is already dead, but an
-- extra 'System.Process.terminateProcess' is applied (this is
-- harmless.)  The background thread with the 'runEffect' is also
-- killed.
--
-- These are all the things a shell does for you.  Makes me a little
-- more grateful for @bash@ and @zsh@ and such.  There are also some
-- Haskell libraries that do some of these things for you; see the
-- @README.md@ file, which is referenced in the main "Pipes.Cliff"
-- module, for some links to those.  Those libraries make other
-- trade-offs, though, which is why I wrote this one.

numsToLess :: IO ExitCode
numsToLess = evalContT $ do
  Mailboxes (Just out) _ _ han <- createProcess
    (proc "less" []) { std_in = Mailbox }
  _ <- background . runEffect $ produceNumbers >-> toOutput out
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
  lift $ waitForProcess lessHan


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
limitedAlphaNumbers = evalContT $ do
  Mailboxes (Just toTr) (Just fromTr) _ _ <- createProcess
    (proc "tr" ["[0-9]", "[a-z]"]) { std_in = Mailbox
                                   , std_out = Mailbox
                                   }
  Mailboxes (Just toCat) _ _ catHan <- createProcess
    (proc "cat" []) { std_in = Mailbox }
  _ <- background . runEffect $ fromInput fromTr >-> toOutput toCat
  _ <- background . runEffect
        $ produceNumbers >-> P.take 300 >-> toOutput toTr
  lift $ waitForProcess catHan
