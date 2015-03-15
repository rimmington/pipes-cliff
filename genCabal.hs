module Main (main) where

import Cartel

props :: Properties
props = blank
  { name = "pipes-subprocess"
  , version = [0,2,0,0]
  , cabalVersion = Just (1, 16)
  , buildType = Just simple
  , license = Just bsd3
  , licenseFile = "LICENSE"
  , licenseFiles = []
  , copyright = "Copyright 2015 Omari Norman"
  , author = "Omari Norman"
  , maintainer = "omari@smileystation.com"
  , stability = "Experimental"
  , homepage = "http://www.github.com/massysett/pipes-subprocess"
  , bugReports = "http://www.github.com/massysett/pipes-subprocess/issues"
  , synopsis = "Streaming to and from subprocesses using Pipes"
  , description =
    [ "pipes-subprocess helps you spawn subprocesses and send data to and"
    , "from them with the Pipes library."
    , "Subprocesses are opened using the"
    , "subprocess library, and the processes and handles are properly"
    , "cleaned up even if there are exceptions."
    , ""
    , "Though this library uses the Pipes library, I have not coordinated"
    , "with the author of the Pipes library in any way.  Any bugs or design"
    , "flaws are mine and should be reported to"
    , ""
    , "<http://www.github.com/massysett/pipes-subprocess/issues>"
    ]
  , category = "Pipes, Concurrency"
  , testedWith = [(ghc, eq [7,8,2])]
  }

defaultOptions :: HasBuildInfo a => [a]
defaultOptions =
  [ haskell2010
  , ghcOptions ["-Wall"]
  ]

library
  :: [NonEmptyString]
  -- ^ Library modules
  -> [LibraryField]
library mods =
  [ exposedModules mods
  , hsSourceDirs ["lib"]
  , libDeps
  ] ++ defaultOptions

sections :: [Section]
sections =
  [ githubHead "massysett" "pipes-subprocess"
  ]

-- # Packages

base :: Package
base = closedOpen "base" [4,7,0,0] [4,8]

transformers :: Package
transformers = closedOpen "transformers" [0,3,0,0] [0,5]

pipes :: Package
pipes = closedOpen "pipes" [4,1] [4,2]

pipesSafe :: Package
pipesSafe = closedOpen "pipes-safe" [2,2] [2,3]

bytestring :: Package
bytestring = closedOpen "bytestring" [0,10,4] [0,11]

process :: Package
process = closedOpen "process" [1,2,0,0] [1,3]

async :: Package
async = closedOpen "async" [2,0] [2,1]

libDeps :: HasBuildInfo a => a
libDeps = buildDepends
  [ base
  , transformers
  , pipes
  , pipesSafe
  , bytestring
  , process
  , async
  ]

main :: IO ()
main = defaultMain $ do
  libMods <- modules "lib"
  return ( props, library libMods, sections )
