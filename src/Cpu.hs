module Main where

import qualified Data.List as List

import Netlist.Jazz
import Cpu.Misc
import Cpu.Alu
import Cpu.Memory
import Cpu.Branch

cpu :: Jazz ()
cpu = do init_registers
         instr <- decode fetch
         (input1, input2) <- alu_inputs instr
         res <- alu instr input1 input2
         branch instr

netlist = build_netlist cpu

main :: IO ()
main = writeFile "cpu.net" $ show netlist

