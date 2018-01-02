module Cpu.Alu where

import Netlist.Jazz
import Cpu.Instr
import Cpu.Misc
import Cpu.Memory
import Cpu.Control
import Cpu.Nalu

import Control.Monad (mapM, mapM_)
import Data.List as List

data Alu_flag = Alu_flag { carry_out :: Bit
                         , is_zero   :: Bit
                         }

-- Liste des controles valant true pour chaque opération
-- Add -> alu_enable_carry, alu_disable_and
-- Sub -> alu_enable_carry, alu_carry_in, alu_invert_y, alu_disable_and
-- And -> alu_invert_y
-- Nor -> alu_invert_x
-- Or  -> alu_force_or, alu_invert_x
-- Xor -> alu_

alu :: (Wr a, Wr b) => Instr -> a -> b -> Jazz (Alu_flag, Wire)
alu instr x y = do
  xs <- bits x
  ys <- bits y
  ctrl <- nalu_control instr
  (c_out, nalu) <- nalu ctrl xs ys
  zero <- wire (32 :: Integer, 0 :: Integer)
  let imm = instr_imm instr
  let ctrl_mux = Opcode_mux { op_j       = wire zero
                            , op_jal     = wire nalu
                            , op_beq     = wire nalu
                            , op_bne     = wire nalu
                            , op_addi    = wire nalu
                            , op_addiu   = wire nalu
                            , op_slti    = wire zero
                            , op_sltiu   = wire zero
                            , op_andi    = wire nalu
                            , op_ori     = wire nalu
                            , op_lui     = conc (List.replicate 16 False) imm
                            , op_lw      = wire zero
                            , op_lbu     = wire zero
                            , op_lhu     = wire zero
                            , op_sb      = wire zero
                            , op_sh      = wire zero
                            , op_sw      = wire zero
                            , op_ll      = wire zero
                            , op_sc      = wire zero

                            , op_sll     = shift True False (instr_shamt instr) x
                            , op_srl     = shift False False (instr_shamt instr) x
                            , op_sra     = shift False True (instr_shamt instr) x
                            , op_jr      = wire zero
                            , op_mfhi    = wire zero
                            , op_mflo    = wire zero
                            , op_mult    = wire zero
                            , op_multu   = wire zero
                            , op_div     = wire zero
                            , op_divu    = wire zero
                            , op_add     = wire nalu
                            , op_addu    = wire nalu
                            , op_sub     = wire nalu
                            , op_subu    = wire nalu
                            , op_and     = wire nalu
                            , op_or      = wire nalu
                            , op_nor     = wire nalu
                            , op_slt     = wire zero
                            , op_sltu    = wire zero

                            , op_nop     = wire zero
                            }
  res <- opcode_mux instr ctrl_mux
  res_zero <- isZero res
  return ( Alu_flag { carry_out = c_out
                    , is_zero = res_zero
                    }
         , res
         )

--doShift :: Instr -> Jazz Bit

nalu_inputs :: Instr -> Jazz (Wire, Wire)
nalu_inputs instr = do
  rs <- read_reg (instr_rs instr)
  rd <- read_reg (instr_rd instr)
  rt <- read_reg (instr_rt instr)
  pc <- reg_out "pc"
  let imm = instr_imm instr
  sign_ext_imm <- conc imm (List.replicate 16 (select 15 imm))
  zero_ext_imm <- conc imm (List.replicate 16 False)
  branch_addr <- conc [False, False] $ conc imm (List.replicate 14 (select 15 imm))
  zero <- wire (32 :: Integer, 0 :: Integer)
  let ctrl_mux = Opcode_mux { op_j       = conc zero zero
                            , op_jal     = conc pc (32 :: Integer, 4 :: Integer)
                            , op_beq     = conc pc branch_addr
                            , op_bne     = conc pc branch_addr
                            , op_addi    = conc rs zero_ext_imm
                            , op_addiu   = conc rs zero_ext_imm
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

                            , op_sll     = conc zero zero
                            , op_srl     = conc zero zero
                            , op_sra     = conc zero zero
                            , op_jr      = conc zero zero
                            , op_mfhi    = conc zero zero
                            , op_mflo    = conc zero zero
                            , op_mult    = conc zero zero
                            , op_multu   = conc zero zero
                            , op_div     = conc zero zero
                            , op_divu    = conc zero zero
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

