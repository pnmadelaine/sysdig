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

-- decodes the instruction
decode :: Wr a => a -> Jazz Instr
decode w = do
   opcode <- bits $ slice 26 32 w
   rs     <- bits $ slice 21 26 w
   rt     <- bits $ slice 16 21 w
   rd     <- bits $ slice 11 16 w
   shamt  <- bits $ slice 6  11 w
   funct  <- bits $ slice 0  6  w
   imm    <- bits $ slice 0  16 w
   addr   <- bits $ slice 0  26 w
   return $ Instr { instr_opcode = opcode
                  , instr_rs     = rs
                  , instr_rt     = rt
                  , instr_rd     = rd
                  , instr_shamt  = shamt
                  , instr_funct  = funct
                  , instr_imm    = imm
                  , instr_addr   = addr
                  }

-- updates the pc at the end of the cycle
branch :: Instr -> Jazz ()
branch instr = do
  pc <- reg_out "pc"
  (_, pc') <- adder (32 :: Integer, 4 :: Integer) pc False
  reg_in "pc" pc'

