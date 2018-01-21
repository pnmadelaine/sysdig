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

handle_netlist1 name = do
  code <- readFile name
  let netlist = read_netlist code
  case schedule netlist of
    Left err      -> putStrLn err
    Right net_sch -> do x <- compile net_sch (-1) [] []
                        writeFile (dropExtension name ++ ".c") x

handle_netlist2 name romname = do
  code <- readFile name
  let netlist = read_netlist code
  rom <- readFile romname
  case schedule netlist of
    Left err      -> putStrLn err
    Right net_sch -> do x <- compile net_sch (-1) [] rom
                        writeFile (dropExtension name ++ ".c") x

main :: IO ()
main = do
  args <- getArgs
  if null args then
    putStrLn "Error: no netlist file specified"
  else
    if null (List.tail args)
    then
      handle_netlist1 (List.head args)
    else
      handle_netlist2 (List.head args) (List.head (List.tail args))
