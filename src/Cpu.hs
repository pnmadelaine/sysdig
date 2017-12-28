module Cpu where

import qualified Data.List as List

import Netlist.Ast
import Netlist.Jazz
import Cpu.Misc
import Cpu.Alu
import Cpu.Memory
import Cpu.Branch

f = Bit (ArgCst [False])
t = Bit (ArgCst [True])

ctrl = Alu_control { alu_enable_carry = t
                   , alu_carry_in     = f
                   , alu_force_or     = t
                   , alu_invert_x     = f
                   , alu_invert_y     = f
                   }

cpu = do init_registers
         instr <- decode fetch
         (input1, input2) <- alu_inputs instr
         ctrl <- get_ctrl_alu instr
         (flags, alu_output) <- alu ctrl input1 input2
         write_reg (List.replicate 5 False) (List.replicate 32 True)
         branch instr
         output "pc_value" (reg_out "pc")

netlist = build_netlist cpu

