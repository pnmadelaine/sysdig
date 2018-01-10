module Cpu.Branch where

import qualified Data.List as List

import Netlist.Jazz

import Cpu.Misc
import Cpu.Instr
import Cpu.Adder
import Cpu.Mult
import Cpu.Memory
import Cpu.Control
import Cpu.Alu

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
branch :: Instr -> Wire -> Jazz ()
branch instr res = do
  let addr = instr_addr instr
  let imm  = instr_imm  instr
  rs <- read_reg (instr_rs instr)
  rt <- read_reg (instr_rt instr)
  not_equal <- nonZero (xor_wire rs rt)
  branch_addr <- conc [False, False] $ conc imm (List.replicate 14 (select 15 imm))
  pc <- reg_out "pc"
  (_, pc_inc) <- adder pc (32 :: Integer, 4 :: Integer) False
  let pc_branch = res
  jump_addr <- conc [False, False] $ conc addr (slice 28 32 pc_inc)
  state <- reg_out "state"
  last_state <- isZero $ xor_wire (6 :: Integer, 32 :: Integer) state
  let branch_mux = Opcode_mux { op_j       = wire jump_addr
                              , op_jal     = wire jump_addr
                              , op_beq     = mux not_equal pc_inc pc_branch
                              , op_bne     = mux not_equal pc_branch pc_inc
                              , op_addi    = wire pc_inc
                              , op_addiu   = wire pc_inc
                              , op_slti    = wire pc_inc
                              , op_sltiu   = wire pc_inc
                              , op_andi    = wire pc_inc
                              , op_ori     = wire pc_inc
                              , op_lui     = wire pc_inc
                              , op_lw      = wire pc_inc
                              , op_lbu     = wire pc_inc
                              , op_lhu     = wire pc_inc
                              , op_sb      = wire pc_inc
                              , op_sh      = wire pc_inc
                              , op_sw      = wire pc_inc
                              , op_ll      = wire pc_inc
                              , op_sc      = wire pc_inc

                              , op_sll     = wire pc_inc
                              , op_srl     = wire pc_inc
                              , op_sra     = wire pc_inc
                              , op_jr      = read_reg (instr_rs instr)
                              , op_mfhi    = wire pc_inc
                              , op_mflo    = wire pc_inc
                              , op_mult    = mux last_state pc_inc pc
                              , op_multu   = mux last_state pc_inc pc
                              , op_div     = mux last_state pc_inc pc
                              , op_divu    = mux last_state pc_inc pc
                              , op_add     = wire pc_inc
                              , op_addu    = wire pc_inc
                              , op_sub     = wire pc_inc
                              , op_subu    = wire pc_inc
                              , op_and     = wire pc_inc
                              , op_or      = wire pc_inc
                              , op_nor     = wire pc_inc
                              , op_slt     = wire pc_inc
                              , op_sltu    = wire pc_inc

                              , op_nop     = wire pc_inc
                              }
  new_pc <- opcode_mux instr branch_mux
  reg_in "pc" new_pc


