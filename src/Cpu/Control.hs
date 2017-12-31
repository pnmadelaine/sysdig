module Cpu.Control where

import Netlist.Jazz

import Cpu.Misc
import Cpu.Instr

import Debug.Trace

data Opcode_mux = Opcode_mux { op_j       :: Jazz Wire
                             , op_jal     :: Jazz Wire
                             , op_beq     :: Jazz Wire
                             , op_bne     :: Jazz Wire
                             , op_addi    :: Jazz Wire
                             , op_addiu   :: Jazz Wire
                             , op_slti    :: Jazz Wire
                             , op_sltiu   :: Jazz Wire
                             , op_andi    :: Jazz Wire
                             , op_ori     :: Jazz Wire
                             , op_lui     :: Jazz Wire
                             , op_lw      :: Jazz Wire
                             , op_lbu     :: Jazz Wire
                             , op_lhu     :: Jazz Wire
                             , op_sb      :: Jazz Wire
                             , op_sh      :: Jazz Wire
                             , op_sw      :: Jazz Wire
                             , op_ll      :: Jazz Wire
                             , op_sc      :: Jazz Wire

                             , op_sll     :: Jazz Wire
                             , op_srl     :: Jazz Wire
                             , op_sra     :: Jazz Wire
                             , op_jr      :: Jazz Wire
                             , op_mfhi    :: Jazz Wire
                             , op_mflo    :: Jazz Wire
                             , op_mult    :: Jazz Wire
                             , op_multu   :: Jazz Wire
                             , op_div     :: Jazz Wire
                             , op_divu    :: Jazz Wire
                             , op_add     :: Jazz Wire
                             , op_addu    :: Jazz Wire
                             , op_sub     :: Jazz Wire
                             , op_subu    :: Jazz Wire
                             , op_and     :: Jazz Wire
                             , op_or      :: Jazz Wire
                             , op_nor     :: Jazz Wire
                             , op_slt     :: Jazz Wire
                             , op_sltu    :: Jazz Wire

                             , op_nop     :: Jazz Wire
                             }

{-
               Opcode_mux { op_j       =
                          , op_jal     =
                          , op_beq     =
                          , op_bne     =
                          , op_addi    =
                          , op_addiu   =
                          , op_slti    =
                          , op_sltiu   =
                          , op_andi    =
                          , op_ori     =
                          , op_lui     =
                          , op_lw      =
                          , op_lbu     =
                          , op_lhu     =
                          , op_sb      =
                          , op_sh      =
                          , op_sw      =
                          , op_ll      =
                          , op_sc      =

                          , op_sll     =
                          , op_srl     =
                          , op_sra     =
                          , op_jr      =
                          , op_mfhi    =
                          , op_mflo    =
                          , op_mult    =
                          , op_multu   =
                          , op_div     =
                          , op_divu    =
                          , op_add     =
                          , op_addu    =
                          , op_sub     =
                          , op_subu    =
                          , op_and     =
                          , op_or      =
                          , op_nor     =
                          , op_slt     =
                          , op_sltu    =

                          , op_nop     =
                          }
                          -}


opcode_mux :: Instr -> Opcode_mux -> Jazz Wire
opcode_mux instr op =
  let aux_i 2 = op_j
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
  let aux_r 0 = op_sll
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
  let f i = if i >= 64 then aux_r (i-64) op
                       else aux_i i op
  in
  let opcode = instr_opcode instr in
  let funct = instr_funct instr in
  do op <- conc (mux (nonZero opcode) opcode funct) [isZero opcode]
     multiplex f op

