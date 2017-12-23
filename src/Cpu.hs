module Cpu where

import Netlist.Ast
import Netlist.Build
import Cpu.Misc
import Cpu.Alu
import Cpu.Memory

ff i = wire [mod i 2 == 1, mod (div i 2) 2 == 1]

cpu = do xs <- input "x" 4
         ys <- input "y" 4
         (_, zs) <- nadder False xs ys
         output "z" zs

netlist = build cpu

