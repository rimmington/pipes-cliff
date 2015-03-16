module Main where

import Pipes.Proctee.Examples
import System.Exit

main :: IO ()
main = numsToLess >>= exitWith
