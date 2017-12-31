module Cpu.Instr where

import Netlist.Jazz

data Instr = Instr { instr_opcode :: [Bit]
                   , instr_rs     :: [Bit]
                   , instr_rd     :: [Bit]
                   , instr_rt     :: [Bit]
                   , instr_shamt  :: [Bit]
                   , instr_funct  :: [Bit]
                   , instr_imm    :: [Bit]
                   , instr_addr   :: [Bit]
                   }

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

