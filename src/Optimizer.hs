module Main where

import Data.List as List

import System.IO
import System.Environment
import System.FilePath

import Netlist.Opt
import Netlist.Show
import Netlist.Parser
import Netlist.Typer

handle_netlist name = do
  code <- readFile (name ++ ".net")
  let netlist = read_netlist code
  let net_opt = optimize netlist
  writeFile (name ++ "_opt.net") (show net_opt)
  case verify net_opt of
    Left err -> putStrLn err
    Right _  -> putStrLn "ok"

main :: IO ()
main = do
  files <- getArgs
  if null files then
    putStrLn "Error: no netlist file specified"
  else do
    let netlist_path = List.head files
    let name = dropExtension netlist_path
    handle_netlist name

