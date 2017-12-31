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

