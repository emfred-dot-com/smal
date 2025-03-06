module Main where

import System.Environment (getArgs)

import Text.Megaparsec (runParser, errorBundlePretty)

import Parser

main :: IO Int
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Usage: smal [script.smal]" >> return 1
    fileName : _ -> do
      fileContents <- readFile fileName
      case runParser pCommand fileName fileContents of
        Left err -> do
          putStrLn (errorBundlePretty err)
          return 1
        Right cmd -> do
          putStrLn (show cmd)
          return 0
