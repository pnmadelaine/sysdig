module Cpu.Branch where

import qualified Data.List as List

import Netlist.Jazz
import Cpu.Misc
import Cpu.Instr
import Cpu.Adder
import Cpu.Mult

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
  state <- reg_out "mult_state"
  b <- multiplying instr /\ nonZero (xor_wire (5 :: Integer, 31 :: Integer) state)
  pc'' <- mux b pc pc'
  reg_in "pc"  pc''

