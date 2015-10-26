module Main where

import           Data.RPN.Api
import           System.Environment
import           System.IO

main :: IO ()
main = do
  args <- getArgs
  inf <- case args of
    [] -> return stdin
    (f:_) -> openFile f ReadMode
  contents <-hGetContents inf
  print $ evaluateCodes (lines contents)
