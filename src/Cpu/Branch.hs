module Cpu.Branch where

import qualified Data.List as List

import Netlist.Jazz

import Cpu.Misc
import Cpu.Instr
import Cpu.Adder
import Cpu.Mult
import Cpu.Memory
import Cpu.Control

-- reads pc and fetches the instruction
fetch :: Jazz Wire
fetch = do
  addr <- bits $ slice 2 32 $ reg_out "pc"
  x0 <- rom $ conc [False, False] addr
  x1 <- rom $ conc [True, False] addr
  x2 <- rom $ conc [False, True] addr
  x3 <- rom $ conc [True, True] addr
  conc x0 $ conc x1 $ conc x2 x3


-- updates the pc at the end of the cycle
branch :: Wr a  => Instr -> a -> Jazz ()
branch instr res = do
  let addr = instr_addr instr
  let imm  = instr_imm  instr
  pc <- reg_out "pc"
  let pc_mux = Opcode_mux { op_j       = wire pc
                          , op_jal     = wire pc
                          , op_beq     = wire res
                          , op_bne     = wire res
                          , op_addi    = wire pc
                          , op_addiu   = wire pc
                          , op_slti    = wire pc
                          , op_sltiu   = wire pc
                          , op_andi    = wire pc
                          , op_ori     = wire pc
                          , op_lui     = wire pc
                          , op_lw      = wire pc
                          , op_lbu     = wire pc
                          , op_lhu     = wire pc
                          , op_sb      = wire pc
                          , op_sh      = wire pc
                          , op_sw      = wire pc
                          , op_ll      = wire pc
                          , op_sc      = wire pc

                          , op_sll     = wire pc
                          , op_srl     = wire pc
                          , op_sra     = wire pc
                          , op_jr      = wire pc
                          , op_mfhi    = wire pc
                          , op_mflo    = wire pc
                          , op_mult    = wire pc
                          , op_multu   = wire pc
                          , op_div     = wire pc
                          , op_divu    = wire pc
                          , op_add     = wire pc
                          , op_addu    = wire pc
                          , op_sub     = wire pc
                          , op_subu    = wire pc
                          , op_and     = wire pc
                          , op_or      = wire pc
                          , op_nor     = wire pc
                          , op_slt     = wire pc
                          , op_sltu    = wire pc

                          , op_nop     = wire pc
                          }
  x <- opcode_mux instr pc_mux
  (_, pc') <- adder (32 :: Integer, 4 :: Integer) x False
  jump_addr <- conc [False, False] $ conc addr (slice 28 32 pc')

  state <- reg_out "mult_state"
  b <- nonZero (xor_wire (5 :: Integer, 31 :: Integer) state)
  let branch_mux = Opcode_mux { op_j       = wire jump_addr
                              , op_jal     = wire jump_addr
                              , op_beq     = wire pc'
                              , op_bne     = wire pc'
                              , op_addi    = wire pc'
                              , op_addiu   = wire pc'
                              , op_slti    = wire pc'
                              , op_sltiu   = wire pc'
                              , op_andi    = wire pc'
                              , op_ori     = wire pc'
                              , op_lui     = wire pc'
                              , op_lw      = wire pc'
                              , op_lbu     = wire pc'
                              , op_lhu     = wire pc'
                              , op_sb      = wire pc'
                              , op_sh      = wire pc'
                              , op_sw      = wire pc'
                              , op_ll      = wire pc'
                              , op_sc      = wire pc'

                              , op_sll     = wire pc'
                              , op_srl     = wire pc'
                              , op_sra     = wire pc'
                              , op_jr      = read_reg (instr_rs instr)
                              , op_mfhi    = wire pc'
                              , op_mflo    = wire pc'
                              , op_mult    = mux b pc pc'
                              , op_multu   = wire pc'
                              , op_div     = wire pc'
                              , op_divu    = wire pc'
                              , op_add     = wire pc'
                              , op_addu    = wire pc'
                              , op_sub     = wire pc'
                              , op_subu    = wire pc'
                              , op_and     = wire pc'
                              , op_or      = wire pc'
                              , op_nor     = wire pc'
                              , op_slt     = wire pc'
                              , op_sltu    = wire pc'

                              , op_nop     = wire pc'
                              }
  pc'' <- opcode_mux instr branch_mux
  reg_in "pc" pc''


