module Cpu.Branch where

import qualified Data.List as List

import Netlist.Jazz
import Cpu.Misc
import Cpu.Adder

-- word_size value
wire_from_integer :: Integer -> Integer -> Jazz Wire
wire_from_integer n x =
  let aux 0 _ = []
      aux n 0 = List.genericReplicate n False
      aux n x = (mod x 2 == 1):(aux (n-1) (div x 2))
  in
  wire (aux n x)

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
  pc <- reg_out "pc"
  (_, pc') <- adder (wire_from_integer 32 4) pc False
  reg_in "pc" pc'

