module Cpu where

import Netlist.Ast
import Netlist.Build
import Cpu.Misc
import Cpu.Alu
import Cpu.Memory

ff i = wire [mod i 2 == 1, mod (div i 2) 2 == 1]

cpu = do new_reg "r" 4
         xs <- input "x" 4
         ys <- reg_out "r"
         reg_in "r" xs
         output "y" ys

netlist = build cpu

