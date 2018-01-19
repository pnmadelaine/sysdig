module Main where

import System.IO
import System.Environment
import System.FilePath.Posix
import System.Console.GetOpt
import Control.Monad (guard, foldM, mapM_)
import Data.List as List

import Assembly.Ast
import Assembly.Parser

handle_assembly name = do
  code <- readFile name
  case read_assembly code of
    Right val ->  putStrLn $ show val
    Left err -> error ("Parsing error: " ++ show err)
  

main :: IO ()
main = do
  args <- getArgs
  if null args then
    putStrLn "Error: no assembly file specified"
  else do
    handle_assembly (List.head args)
