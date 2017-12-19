module Main where

import Netlist.Ast
import Netlist.Show

import qualified Cpu

netlist = Cpu.netlist

main :: IO ()
main = writeFile "cpu.net" $ show netlist

