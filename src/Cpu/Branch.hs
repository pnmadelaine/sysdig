module Cpu.Branch where

import qualified Data.List as List

import Netlist.Jazz
import Cpu.Misc
import Cpu.Instr
import Cpu.Adder

-- reads pc and fetches the instruction
fetch :: Jazz Wire
fetch = do
  addr <- bits $ slice 2 32 $ reg_out "pc"
  x0 <- rom $ conc [False, False] addr
  x1 <- rom $ conc [True, False] addr
  x2 <- rom $ conc [False, True] addr
  x3 <- rom $ conc [True, True] addr
  conc x0 $ conc x1 $ conc x2 x3


-- updates the pc at the end of the cycle
branch :: Instr -> Jazz ()
branch instr = do
  pc <- reg_out "pc"
  (_, pc') <- adder (32 :: Integer, 4 :: Integer) pc False
  reg_in "pc" pc'

