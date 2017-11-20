module Main where

import Netlist.Ast
import Netlist.Show
import Netlist.Build
import Netlist.Typer

import qualified Alu

netlist = Alu.netlist

main :: IO ()
main = do writeFile "alu.net" (show netlist)
          case verify netlist of
            Left err -> putStrLn err
            Right _  -> return ()

