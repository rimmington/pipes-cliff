module Main where

import Pipes.Cliff.Examples
import System.Exit

main :: IO ()
main = limitedAlphaNumbers
--main = limitedAlphaNumbers >>= exitWith
