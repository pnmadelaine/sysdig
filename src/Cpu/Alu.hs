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

alu_inputs :: Instr -> Jazz (Wire, Wire)
alu_inputs instr = do
  input1    <- read_reg (instr_rs instr)
  value_rt  <- read_reg (instr_rt instr)
  signed    <- extension_mode instr
  immediate <- extend signed 16 (instr_imm instr)
  input2    <- mux (imm_ctrl instr) immediate value_rt
  return (input1, input2)

