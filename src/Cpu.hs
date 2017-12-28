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

ctrl = Alu_control { alu_enable_carry = f
                   , alu_carry_in     = f
                   , alu_force_or     = t
                   , alu_invert_x     = f
                   , alu_invert_y     = f
                   }

cpu = do init_registers
         instr <- decode fetch
         write_reg (List.replicate 5 False) (List.replicate 32 True)
         branch instr

netlist = build cpu

