module Cpu.Mult where

import Netlist.Jazz

import Cpu.Misc
import Cpu.Adder
import Cpu.Instr

multiplying :: Instr -> Jazz Bit
multiplying instr = isZero (instr_opcode instr) -- TODO: with a multiplexer
                 /\ ( isZero (xor_wire (5 :: Integer, 24 :: Integer) (instr_funct instr))
                      \/ isZero (xor_wire (5 :: Integer, 25 :: Integer) (instr_funct instr)) )

mult_update_hilo :: Instr -> (Bit, Wire) -> Jazz ()
mult_update_hilo instr (c_out, res) = do
  state <- reg_out "mult_state"
  hilo  <- reg_out "hilo"
  hilo' <- conc (slice 1 32 hilo) $ conc res [c_out]
  reg_in "hilo" (mux (multiplying instr) hilo' hilo)


mult_get_inputs :: (Wr a, Wr b) => Instr -> a -> b -> Jazz Wire
mult_get_inputs instr rs rt = do
  acc   <- reg_out "mult_acc"
  state <- reg_out "mult_state"
  hilo  <- reg_out "hilo"

  x      <- mux (isZero state) rt acc
  (_, y) <- adder (5 :: Integer, 0 :: Integer) state True
  z      <- mux (select 0 x) rs (32 :: Integer, 0 :: Integer)

  acc'   <- right_shift x (5 :: Integer, 1 :: Integer)
  state' <- mux (multiplying instr) y state
  reg_in "mult_state" state'
  reg_in "mult_acc" acc'

  conc (slice 32 64 hilo) z

