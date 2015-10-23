module Main where

import           Data.RPN.Api
import           System.Console.GetOpt
import           System.Environment
import           System.IO

-- | abstract values for the flags
data Flag = TokenizeOnly -- optional
          | ParseOnly    -- optional
          deriving (Show)
-- | descriptions of the flags
options:: [OptDescr Flag]
options =
  [ Option "t" ["tokenize"] (NoArg TokenizeOnly) "output list of tokens"
  , Option "p" ["parse"]    (NoArg ParseOnly)    "output syntax tree"
  ]


main :: IO ()
main = do
  args <- getArgs
  case getOpt Permute options args of
    (_,_,msgs@(_:_))       -> error $ concat msgs ++ usageMsg
    (flags,fileNames,_)    -> handleFlags flags fileNames


handleFlags:: [Flag] -> [FilePath]   -> IO ()
handleFlags flgs files = do
  hinp <- case files of
               [] -> return stdin
               (i:_) -> openFile i ReadMode
  hout <- case files of
               []      -> return stdout
               (_:o:_) -> openFile o WriteMode
               [_]     -> return stdout

  input <- takeWhile (not.null).lines <$> hGetContents hinp
  case flgs of
        (TokenizeOnly:_) -> mapM_ (hPrint hout.tokenize)  input
        (ParseOnly:_)    -> sequence_ $ foldl (\b a->b `mappend` map (hPutStrLn hout) a) [] (map (printTree.parse.tokenize) input)
--mapM_ (hPrint hout) (map (printTree.parse.tokenize) input)
        _                -> mapM_ (hPrint hout.emitInstructions.generate.parse.tokenize) input

  print input
  hClose hinp



usageMsg::String
usageMsg = usageInfo "yarpnc <inputFile> <outputFile> [OPTIONS]" options
--  input <- fmap (map  (emitInstructions.generate.parse.tokenize)  ) (takeWhile (not.null).lines <$> getContents)
--  input <- parse.takeWhile (not.null).lines <$>   getContents
--  print input
--     input <- takeWhile (not.null).lines <$> getContents


  --     input <- takeWhile (/= "q").lines <$> getContents
--     print $ map (parse.tokenize) input
--     print (concatMap tokenize input)
{-
loop :: IO ()
loop = do
  args <- getArgs
  line <- getLine
  eof <- isEOF
  unless (eof || null line) $
    do
     c <- processLine line
     mapM_ putStrLn c
     loop

-}
{-
processLine::String -> IO [String]
processLine s = return $ (emitInstructions.generate.parse.tokenize) s
-}
