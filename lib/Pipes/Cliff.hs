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
-- You will want to consult "Pipes.Cliff.Examples" for some examples
-- before getting started.  There are some important notes in there
-- about how to run pipelines.
module Pipes.Cliff
  ( -- * Specifying a subprocess's properties
    CmdSpec(..)
  , NonPipe(..)
  , CreateProcess(..)
  , procSpec
  , squelch

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

  -- * Background operations

  -- | Often it is necessary to run threads in the background; in
  -- addition, all subprocesses run in the background.  These
  -- functions allow you to launch threads in the background and to
  -- wait on background threads and subprocesses.

  , conveyor
  , background
  , waitForProcess
  , waitForThread

  -- * Running a single 'Effect'
  , runCliff

  -- * Errors and warnings

  -- | You will only need what's in this section if you want to
  -- examine errors more closely.
  , Activity(..)
  , HandleDesc(..)
  , HandleOopsie(..)
  , Oopsie(..)

  -- * Re-exports
  -- $reexports
  , module Control.Concurrent.MVar
  , module Pipes
  , module Pipes.Safe
  , module System.Exit
  , module System.Process

  -- * Some design notes
  -- $designNotes
  ) where

import Pipes.Cliff.Core
import Pipes
import Pipes.Safe
import System.Exit
import System.Process (ProcessHandle)
import Control.Concurrent.MVar

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
subprocess that creates a 'Proxy' for standard input and standard
output, use 'pipeInputOutput'.  You must describe what you want done
with standard error.  A 'Producer' is returned for standard output
and a 'Consumer' for standard input.

If you are creating a 'Proxy' for only one stream (for instance,
you're using 'pipeOutput') then a single 'Proxy' is returned to you.
That 'Proxy' manages all the resources it creates; so, for example,
when you ultimately run your 'Effect', the process is created and then
destroyed when the 'MonadSafe' computation completes.

If you are creating a 'Proxy' for more than one stream (for instance,
you're using 'pipeInputOutput') then the multiple 'Proxy' are returned
to you in a tuple in the 'MonadSafe' computation.  The 'MonadSafe'
computation will make sure that the resulting process and handles are
destroyed when you exit the 'MonadSafe' computation.  In such a case,
you must make sure that you don't try to use the streams outside of
the 'MonadSafe' computation, because the subprocess will already be
destroyed.  To make sure you are done using the streams before leaving
the 'MonadSafe' computation, you can run the 'Effect' you're
interested in the the main thread, or perhaps you may want to use
'storeProcessHandle' and then use 'waitForProcess' to wait for any
critical process to complete.

-}

{- $reexports

   * "Control.Concurrent.MVar" reexports all bindings

   * "Pipes" reexports all bindings

   * "Pipes.Safe" reexports all bindings

   * "System.Exit" reexports all bindings

   * "System.Process" reexports 'ProcessHandle'

-}

{- $designNotes

Two overarching principles guided the design of this
library. First, I wanted the interface to use simple
ByteStrings.  That most closely represents what a UNIX process
sees.  If the user wants to use Text or String, it's easy
enough to convert between those types and a ByteString.  Then
the user has to pay explicit attention to encoding issues--as
she should, because not all UNIX processes deal with encoded
textual data.

Second, I paid meticulous attention to resource management.
Resources are deterministically destroyed immediately after
use.  This eliminates many bugs.

You might wonder why, if you are using an external process as
a pipeline, why can't you create, well, a 'Pipe'?  Wouldn't
that be an obvious choice?  Well, if such an interface is
possible using Pipes in its current incarnation, nobody has
figured it out yet.  I don't think it's possible.  See also

<https://groups.google.com/d/msg/haskell-pipes/JFfyquj5HAg/Lxz7p50JOh4J>

for a discussion about this.

-}
