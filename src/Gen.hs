module Main where

import Netlist.Ast
import Netlist.Show

import qualified Alu

netlist = Alu.netlist

main :: IO ()
main = writeFile "alu.net" $ show netlist

