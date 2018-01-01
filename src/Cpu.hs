module Main where

import qualified Data.List as List

import Netlist.Ast
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
         (_, z) <- alu instr input1 input2
         output "z" z
         write_reg (output_reg instr) z
         branch instr

netlist = build_netlist cpu
netlist' = netlist { netlist_out = netlist_out netlist ++ List.tail registers_names ++ ["pc"] } 

main :: IO ()
main = writeFile "cpu.net" $ show netlist'

