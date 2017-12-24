module Cpu.Branch where

import qualified Data.List as List

import Netlist.Jazz
import Cpu.Misc

-- reads pc and fetches the instruction
fetch :: Jazz Wire
fetch = do
  addr <- slice 2 32 $ reg_out "pc"
  x0 <- rom $ conc [False, False] addr
  x1 <- rom $ conc [True, False] addr
  x2 <- rom $ conc [False, True] addr
  x3 <- rom $ conc [True, True] addr
  conc x0 $ conc x1 $ conc x2 x3

-- updates the pc at the end of the cycle
branch :: Instr -> Jazz ()
branch instr = do
  xs <- reg_out "pc"
  ys <- bits xs -- todo: branching 
  reg_in "pc" ys

