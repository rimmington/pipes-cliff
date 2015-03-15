# pipes-process

pipes-process helps you spawn subprocesses and send data to and
from them with the Pipes library.  Subprocesses are opened using the
process library, and the processes and handles are properly cleaned
up even if there are exceptions.

Though this library uses the Pipes library, I have not coordinated
with the author of the Pipes library in any way.  Any bugs or design
flaws are mine and should be reported to

http://www.github.com/massysett/pipes-process/issues

Though I hope pipes-process works on Windows systems, I have only
tested it on Unix-like systems.  Any reports on how this library works
on Windows are appreciated.

## Building pipes-process

If you get the files from Hackage, just build them in the usual way
using cabal-install.

To build the files from Github, you will need to generate the Cabal
file.  The Cabal file is generated using the Cartel package, so first
run

cabal install cartel

and then, from the main directory (the one with the generate script),
run

sh generate

(at least, this is how you would do it on a Unix-like system.)
