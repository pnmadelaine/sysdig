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
         (flags, res) <- alu_output instr
         write_reg (output_reg instr) res
         branch instr res

netlist = build_netlist cpu
netlist' = netlist { netlist_out = netlist_out netlist ++ List.tail registers_names
                                                       ++ ["pc", "hi", "lo", "state"]
                   }

main :: IO ()
main = writeFile "cpu.net" $ show netlist'

