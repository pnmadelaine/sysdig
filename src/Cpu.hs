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
         output "secondes" $ reg_out "t0"
         output "minutes" $ reg_out "t1"
         output "heures" $ reg_out "t2"
         output "jour" $ reg_out "t3"
         output "mois" $ reg_out "t5"
         output "annee" $ reg_out "t6" 

netlist = build_netlist cpu

main :: IO ()
main = writeFile "cpu.net" $ show netlist

