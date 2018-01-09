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

stripThisDamnedExtension :: String -> String
stripThisDamnedExtension [] = ""
stripThisDamnedExtension str = List.reverse (aux (List.reverse str))
  where aux [] = []
        aux ('.':cs) = cs
        aux (c:cs) = c:(aux cs)

main :: IO ()
main = do
  files <- getArgs
  if null files then
    putStrLn "Error: no netlist file specified"
  else do
    handle_netlist (stripThisDamnedExtension (List.head files))

