module Main where

import qualified Data.ByteString as BS
import           System.Environment
import           System.IO

import           DotGraphCompiler

main :: IO ()
main =
  do
    args <- getArgs
    case args of
      [file] -> doFile file
      _ -> do
        name <- getProgName
        hPutStrLn stderr $ "usage: " ++ name ++ " [FILENAME]"

doFile :: FilePath -> IO ()
doFile filename = do
  bs <- BS.readFile filename
  case compile bs of
    Left e  -> error e
    Right s -> putStr s
