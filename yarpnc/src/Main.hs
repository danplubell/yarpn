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
        (TokenizeOnly:_) -> sequence_ $ foldl (\b a->b `mappend` map (hPrint hout) a) [] (map tokenize input)
        (ParseOnly:_)    -> sequence_ $ foldl (\b a->b `mappend` map (hPutStrLn hout) a) [] (map (printTree.parse.tokenize) input)
        _                -> sequence_ $ foldl (\b a->b `mappend` map (hPutStrLn hout) a) [] (map (emitInstructions.generate.parse.tokenize) input)

  hClose hinp
  hClose hout


usageMsg::String
usageMsg = usageInfo "yarpnc <inputFile> <outputFile> [OPTIONS]" options
