module Cpu where

import qualified Data.List as List

import Netlist.Ast
import Netlist.Jazz
import Cpu.Misc
import Cpu.Alu
import Cpu.Memory
import Cpu.Branch

print_alu_ctrl ctrl = do
  output "enable_carry" [alu_enable_carry ctrl]
  output "carry_in" [alu_carry_in ctrl]
  output "force_or" [alu_force_or ctrl]
  output "invert_x" [alu_invert_x ctrl]
  output "invert_y" [alu_invert_y ctrl]

cpu = do init_registers
         instr <- decode fetch
         (input1, input2) <- alu_inputs instr
         ctrl <- alu_control instr
         (flags, alu_output) <- alu ctrl input1 input2
         write_reg (List.replicate 5 False) (List.replicate 32 True)
         branch instr

netlist = build_netlist cpu

