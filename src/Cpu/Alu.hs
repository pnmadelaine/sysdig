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
  (c_out, zs) <- nalu ctrl xs ys
  z <- wire zs
  zero <- isZero z
  return ( Alu_flag { carry_out = c_out
                    , is_zero = zero
                    }
         , z
         )

--doShift :: Instr -> Jazz Bit

--decides how immediate is extended
extension_mode :: Instr -> Jazz Bit
extension_mode instr =
  select 5 (instr_opcode instr) \/ neg (select 2 (instr_opcode instr))

--decides which is input of the ALU between rt and immediate
--imm_ctrl is true if ALU reads immediate value
imm_ctrl :: Instr -> Jazz Bit
imm_ctrl instr = do
  --every instruction that read rt have an opcode < 8
  y <- slice 3 6 (instr_opcode instr)
  nonZero y

nalu_inputs :: Instr -> Jazz (Wire, Wire)
nalu_inputs instr = do
  rs <- read_reg (instr_rs instr)
  rd <- read_reg (instr_rd instr)
  rt <- read_reg (instr_rt instr)
  pc <- reg_out "pc"
  signed    <- extension_mode instr
  imm <- extend signed 16 (instr_imm instr)
  let zero = wire (5 :: Integer, 0 :: Integer)
  let ctrl_mux = Opcode_mux { op_j       = conc zero zero
                            , op_jal     = conc pc (32 :: Integer, 8 :: Integer)
                            , op_beq     = conc rs rt
                            , op_bne     = conc rs rt
                            , op_addi    = conc rs    imm
                            , op_addiu   = conc rs    imm
                            , op_slti    = conc rs    imm
                            , op_sltiu   = conc rs    imm
                            , op_andi    = conc rs    imm
                            , op_ori     = conc rs    imm
                            , op_lui     = conc zero zero
                            , op_lw      = conc rs    imm
                            , op_lbu     = conc rs    imm
                            , op_lhu     = conc rs    imm
                            , op_sb      = conc rs    imm
                            , op_sh      = conc rs    imm
                            , op_sw      = conc rs    imm
                            , op_ll      = conc rs    imm
                            , op_sc      = conc rs    imm

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
  input1 <- read_reg $ slice 0 5 inputs
  input2 <- read_reg $ slice 5 10 inputs

  return (input1, input2)

