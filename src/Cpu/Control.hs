module Cpu.Control where

import Netlist.Jazz

import Cpu.Misc
import Cpu.Instr

data Opcode_mux = Opcode_mux { op_j       :: Wire
                             , op_jal     :: Wire
                             , op_beq     :: Wire
                             , op_bne     :: Wire
                             , op_addi    :: Wire
                             , op_addiu   :: Wire
                             , op_slti    :: Wire
                             , op_sltiu   :: Wire
                             , op_andi    :: Wire
                             , op_ori     :: Wire
                             , op_lui     :: Wire
                             , op_lw      :: Wire
                             , op_lbu     :: Wire
                             , op_lhu     :: Wire
                             , op_sb      :: Wire
                             , op_sh      :: Wire
                             , op_sw      :: Wire
                             , op_ll      :: Wire
                             , op_sc      :: Wire

                             , op_sll     :: Wire
                             , op_srl     :: Wire
                             , op_sra     :: Wire
                             , op_jr      :: Wire
                             , op_mfhi    :: Wire
                             , op_mflo    :: Wire
                             , op_mult    :: Wire
                             , op_multu   :: Wire
                             , op_div     :: Wire
                             , op_divu    :: Wire
                             , op_add     :: Wire
                             , op_addu    :: Wire
                             , op_sub     :: Wire
                             , op_subu    :: Wire
                             , op_and     :: Wire
                             , op_or      :: Wire
                             , op_nor     :: Wire
                             , op_slt     :: Wire
                             , op_sltu    :: Wire

                             , op_nop     :: Wire
                             }

opcode_def :: Wr a => a -> Jazz Opcode_mux
opcode_def x = wire x >>= \ v -> return $
               Opcode_mux { op_j       = v
                          , op_jal     = v
                          , op_beq     = v
                          , op_bne     = v
                          , op_addi    = v
                          , op_addiu   = v
                          , op_slti    = v
                          , op_sltiu   = v
                          , op_andi    = v
                          , op_ori     = v
                          , op_lui     = v
                          , op_lw      = v
                          , op_lbu     = v
                          , op_lhu     = v
                          , op_sb      = v
                          , op_sh      = v
                          , op_sw      = v
                          , op_ll      = v
                          , op_sc      = v

                          , op_sll     = v
                          , op_srl     = v
                          , op_sra     = v
                          , op_jr      = v
                          , op_mfhi    = v
                          , op_mflo    = v
                          , op_mult    = v
                          , op_multu   = v
                          , op_div     = v
                          , op_divu    = v
                          , op_add     = v
                          , op_addu    = v
                          , op_sub     = v
                          , op_subu    = v
                          , op_and     = v
                          , op_or      = v
                          , op_nor     = v
                          , op_slt     = v
                          , op_sltu    = v

                          , op_nop     = v
                          }

opcode_mux :: Instr -> Opcode_mux -> Jazz Wire
opcode_mux instr op =
  let aux_i :: Integer -> Opcode_mux -> Wire
      aux_i 2 = op_j
      aux_i 3 = op_jal
      aux_i 4 = op_beq
      aux_i 5 = op_bne
      aux_i 8 = op_addi
      aux_i 9 = op_addiu
      aux_i 10 = op_slti
      aux_i 11 = op_sltiu
      aux_i 12 = op_andi
      aux_i 13 = op_ori
      aux_i 15 = op_lui
      aux_i 35 = op_lw
      aux_i 36 = op_lbu
      aux_i 37 = op_lhu
      aux_i 40 = op_sb
      aux_i 41 = op_sh
      aux_i 43 = op_sw
      aux_i 48 = op_ll
      aux_i 56 = op_sc
      aux_i _ = op_nop
  in
  let aux_r :: Integer -> Opcode_mux -> Wire
      aux_r 0 = op_sll
      aux_r 2 = op_srl
      aux_r 3 = op_sra
      aux_r 8 = op_jr
      aux_r 16 = op_mfhi
      aux_r 18 = op_mflo
      aux_r 24 = op_mult
      aux_r 25 = op_multu
      aux_r 26 = op_div
      aux_r 27 = op_divu
      aux_r 32 = op_add
      aux_r 33 = op_addu
      aux_r 34 = op_sub
      aux_r 35 = op_subu
      aux_r 36 = op_and
      aux_r 37 = op_or
      aux_r 39 = op_nor
      aux_r 42 = op_slt
      aux_r 43 = op_sltu
      aux_r _ = op_nop
  in
  let f i = if i >= 64 then wire $ aux_r (i-64) op
                       else wire $ aux_i i op
  in
  let opcode = instr_opcode instr in
  let funct = instr_funct instr in
  let op = conc (mux (nonZero opcode) opcode funct) [isZero opcode] in
  multiplex f (mux (isZero opcode) funct opcode)

