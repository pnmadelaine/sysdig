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
         write_reg (List.replicate 5 False) (List.replicate 32 True) -- 
         branch instr
         output "pc_value" (reg_out "pc")

foo = do xs <- bits $ input "x" 4
         ys <- bits $ input "y" 4
         (_, zs) <- alu ctrl xs ys
         output "z" zs

foo' = do xs <- bits $ input "x" 4
          output "y" xs

netlist = build_netlist foo

