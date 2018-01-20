module Main where

import System.IO
import System.Environment
import System.FilePath.Posix
import System.Console.GetOpt
import Control.Monad (guard, foldM, mapM_)
import Data.List as List

import Assembly.Ast
import Assembly.Parser
import Assembly.Compiler

handle_assembly :: String -> IO ()
handle_assembly name = do
  code <- readFile name
  case read_assembly code of
    Right p -> do let up = understand_assembly p
                  --putStrLn $ print_prog up
                  let bname = List.take ( (List.length name) -2 ) name
                  writeFile bname (print_prog up)
    Left err -> error ("Parsing error: " ++ show err)
  

main :: IO ()
main = do
  args <- getArgs
  if null args then
    putStrLn "Error: no assembly file specified"
  else do
    handle_assembly (List.head args)
