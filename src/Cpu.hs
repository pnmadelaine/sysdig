module Cpu where

import Netlist.Ast
import Netlist.Jazz
import Cpu.Misc
import Cpu.Alu
import Cpu.Memory

f = Bit (ArgCst [False])
t = Bit (ArgCst [True])

ctrl = Alu_control { alu_enable_carry = f
                   , alu_carry_in     = f
                   , alu_enable_xor   = t
                   , alu_enable_and   = t
                   , alu_invert_x     = f
                   , alu_invert_y     = f
                   }

cpu = do xs <- bits $ input "x" 4
         ys <- bits $ input "y" 4
         (_, zs, _) <- nalu ctrl xs ys False
         output "z" zs

netlist = build cpu

