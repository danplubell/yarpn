module Main where

import           Data.RPN.Api
import           System.IO

main :: IO ()
main = loop

loop :: IO ()
loop = do
  line <- getLine
  eof <- isEOF
  if eof || null line
     then return ()
     else do
          c <- processLine line
          mapM_ putStrLn c
          loop


processLine::String -> IO [String]
processLine s = return $ (emitInstructions.generate.parse.tokenize) s
