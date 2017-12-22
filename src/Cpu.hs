module Cpu where

import Netlist.Ast
import Netlist.Build
import Cpu.Misc
import Cpu.Alu

f i = prog [mod i 2 == 1]

cpu = do xs <- input "x" 8
         ys <- input "y" 8
         (_, zs) <- nadder False xs ys
         output "z" zs

netlist = build cpu

