module Main where

import qualified Data.List as List

import Netlist.Jazz
import Cpu.Instr
import Cpu.Misc
import Cpu.Alu
import Cpu.Memory
import Cpu.Branch

cpu :: Jazz ()
cpu = do init_registers
         instr <- decode fetch
         (input1, input2) <- nalu_inputs instr
         (_, z) <- alu instr (32 :: Integer, 0 :: Integer) (32 :: Integer, 0 :: Integer)
         output "z" z
         branch instr

netlist = build_netlist cpu

main :: IO ()
main = writeFile "cpu.net" $ show netlist

