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
                   , alu_enable_xor   = t
                   , alu_enable_and   = f
                   , alu_invert_x     = f
                   , alu_invert_y     = f
                   }

cpu = do init_registers
         instr <- decode fetch
         write_reg (List.replicate 5 False) (List.replicate 32 True)
         branch instr

foo = do xs <- bits $ input "x" 4
         ys <- bits $ input "y" 4
         (_, zs) <- alu ctrl xs ys
         output "z" zs

foo' = do new_reg "x" 2
          output "x_out" (reg_out "x")
          reg_in "x" $ conc [True] [True]

netlist = build foo

