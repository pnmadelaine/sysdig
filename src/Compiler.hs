module Main where

import System.IO
import System.Environment
import System.FilePath.Posix
import System.Console.GetOpt
import Control.Monad (guard, foldM, mapM_)
import Data.List as List

import Netlist.Ast
import Netlist.Compiler
import Netlist.Typer
import Netlist.Scheduler
import Netlist.Parser

import qualified Cpu

handle_netlist name = do
  code <- readFile name
  let netlist = read_netlist code
  case schedule netlist of
    Left err      -> putStrLn err
    Right net_sch -> compile net_sch

main :: IO ()
main = do
  args <- getArgs
  if null args then
    putStrLn "Error: no netlist file specified"
  else do
    handle_netlist (List.head args)
