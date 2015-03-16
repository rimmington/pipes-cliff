module Main where

import Pipes.Process.Examples
import System.Exit

main :: IO ()
main = numsToLess >>= exitWith
