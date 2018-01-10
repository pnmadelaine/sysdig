module Cpu.Alu where

import Netlist.Jazz
import Cpu.Instr
import Cpu.Misc
import Cpu.Memory
import Cpu.Control
import Cpu.Nalu
import Cpu.Adder
import Cpu.Mult

import Control.Monad (mapM, mapM_)
import Data.List as List

data Alu_flag = Alu_flag { carry_out :: Bit
                         , is_zero   :: Bit
                         , not_equal :: Bit
                         }

-- Liste des controles valant true pour chaque opération
-- Add -> alu_enable_carry, alu_disable_and
-- Sub -> alu_enable_carry, alu_carry_in, alu_invert_y, alu_disable_and
-- And -> alu_invert_y
-- Nor -> alu_invert_x
-- Or  -> alu_force_or, alu_invert_x
-- Xor -> alu_

alu_output :: Instr -> Jazz (Alu_flag, Wire)
alu_output instr = do
  (x, y)            <- alu_inputs instr
  ctrl              <- nalu_control instr
  (c_out, nalu_out) <- nalu ctrl x y

  zero      <- wire (32 :: Integer, 0 :: Integer)
  let shamt = instr_shamt instr
  let imm   = instr_imm instr
  ram_out   <- handle_ram instr nalu_out (read_reg $ instr_rt instr)

  let res_mux = Opcode_mux { op_j       = wire zero
                           , op_jal     = wire nalu_out
                           , op_beq     = wire nalu_out
                           , op_bne     = wire nalu_out
                           , op_addi    = wire nalu_out
                           , op_addiu   = wire nalu_out
                           , op_slti    = wire zero
                           , op_sltiu   = conc [neg c_out] (31 :: Integer, 0 :: Integer)
                           , op_andi    = wire nalu_out
                           , op_ori     = wire nalu_out
                           , op_lui     = conc (List.replicate 16 False) imm
                           , op_lw      = wire ram_out
                           , op_lbu     = conc (slice 0 8 ram_out) (24 :: Integer, 0 :: Integer)
                           , op_lhu     = conc (slice 0 16 ram_out) (16 :: Integer, 0 :: Integer)
                           , op_sb      = wire zero
                           , op_sh      = wire zero
                           , op_sw      = wire zero
                           , op_ll      = wire zero -- huh?
                           , op_sc      = wire zero

                           , op_sll     = left_shift x shamt
                           , op_srl     = right_shift x shamt
                           , op_sra     = right_arith_shift x shamt
                           , op_jr      = wire zero
                           , op_mfhi    = reg_out "hi"
                           , op_mflo    = reg_out "lo"
                           , op_mult    = wire nalu_out
                           , op_multu   = wire nalu_out
                           , op_div     = wire nalu_out
                           , op_divu    = wire nalu_out
                           , op_add     = wire nalu_out
                           , op_addu    = wire nalu_out
                           , op_sub     = wire nalu_out
                           , op_subu    = wire nalu_out
                           , op_and     = wire nalu_out
                           , op_or      = wire nalu_out
                           , op_nor     = wire nalu_out
                           , op_slt     = wire zero
                           , op_sltu    = conc [neg c_out] (31 :: Integer, 0 :: Integer)

                           , op_nop     = wire zero
                           }
  res       <- opcode_mux instr res_mux
  res_zero  <- isZero res
  neq       <- nonZero (xor_wire x y)
  let flags = Alu_flag { carry_out = c_out
                       , is_zero   = res_zero
                       , not_equal = neq
                       }

  update_state instr (c_out, res)
  return (flags, res)

alu_inputs :: Instr -> Jazz (Wire, Wire)
alu_inputs instr = do
  rs <- read_reg (instr_rs instr)
  rd <- read_reg (instr_rd instr)
  rt <- read_reg (instr_rt instr)
  pc <- reg_out "pc"
  let imm = instr_imm instr
  sign_ext_imm <- conc imm (List.replicate 16 (select 15 imm))
  zero_ext_imm <- conc imm (List.replicate 16 False)
  branch_addr  <- conc [False, False] $ conc imm (List.replicate 14 (select 15 imm))
  zero <- wire (32 :: Integer, 0 :: Integer)
  hi <- reg_out "hi"
  lo <- reg_out "lo"
  (_, pc_inc) <- adder (reg_out "pc") (32 :: Integer, 4 :: Integer) False
  let ctrl_mux = Opcode_mux { op_j       = conc zero zero
                            , op_jal     = conc pc (32 :: Integer, 8 :: Integer)
                            , op_beq     = conc pc_inc branch_addr
                            , op_bne     = conc pc_inc branch_addr
                            , op_addi    = conc rs sign_ext_imm
                            , op_addiu   = conc rs sign_ext_imm
                            , op_slti    = conc rs sign_ext_imm
                            , op_sltiu   = conc rs sign_ext_imm
                            , op_andi    = conc rs zero_ext_imm
                            , op_ori     = conc rs zero_ext_imm
                            , op_lui     = conc zero zero
                            , op_lw      = conc rs sign_ext_imm
                            , op_lbu     = conc rs sign_ext_imm
                            , op_lhu     = conc rs sign_ext_imm
                            , op_sb      = conc rs sign_ext_imm
                            , op_sh      = conc rs sign_ext_imm
                            , op_sw      = conc rs sign_ext_imm
                            , op_ll      = conc rs sign_ext_imm
                            , op_sc      = conc rs sign_ext_imm

                            , op_sll     = conc rt zero
                            , op_srl     = conc rt zero
                            , op_sra     = conc rt zero
                            , op_jr      = conc zero zero
                            , op_mfhi    = conc zero zero
                            , op_mflo    = conc zero zero
                            , op_mult    = conc hi rt
                            , op_multu   = conc hi rt
                            , op_div     = conc (conc [select 31 lo] (slice 0 31 hi)) rt
                            , op_divu    = conc (conc [select 31 lo] (slice 0 31 hi)) rt
                            , op_add     = conc rs rt
                            , op_addu    = conc rs rt
                            , op_sub     = conc rs rt
                            , op_subu    = conc rs rt
                            , op_and     = conc rs rt
                            , op_or      = conc rs rt
                            , op_nor     = conc rs rt
                            , op_slt     = conc rs rt
                            , op_sltu    = conc rs rt

                            , op_nop     = conc zero zero
                            }
  inputs <- opcode_mux instr ctrl_mux
  input1 <- slice 0 32 inputs
  input2 <- slice 32 64 inputs

  return (input1, input2)

