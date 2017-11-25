module Main where

import Netlist.Ast
import Netlist.Show

import qualified Alu

netlist = Alu.netlist

main :: IO ()
main = writeFile "test.net" $ show netlist

