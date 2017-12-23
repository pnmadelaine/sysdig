module Cpu where

import Netlist.Ast
import Netlist.Build
import Cpu.Misc
import Cpu.Alu
import Cpu.Memory

ff i = wire [mod i 2 == 1, mod (div i 2) 2 == 1]

cpu = do init_registers
         xs <- input "addr" 5
         ys <- read_reg xs
         output "data" ys

netlist = build cpu

