module Cpu.Misc where

import Netlist.Jazz

import Control.Monad (mapM, mapM_)
import Data.List as List

data Instr = Instr { instr_opcode :: [Bit]
                   , instr_rs     :: [Bit]
                   , instr_rd     :: [Bit]
                   , instr_rt     :: [Bit]
                   , instr_shamt  :: [Bit]
                   , instr_funct  :: [Bit]
                   , instr_imm    :: [Bit]
                   , instr_addr   :: [Bit]
                   }

multiplex :: Wr a => (Integer -> Jazz Wire) -> a -> Jazz Wire
multiplex f x =
  let aux :: Bt a => Integer -> Integer -> [a] -> Jazz Wire
      aux _ j [] = f j
      aux i j (x:xs) =
        mux x
          (aux (2*i) (j+i) xs)
          (aux (2*i) j     xs)
  in
  bits x >>= \l -> aux 1 0 l

-- direction number_of_shifts value
shift :: (Bt a, Bt b, Bt c) => a -> [b] -> [c] -> Jazz [Bit]
shift a ws xs =
  let aux :: (Bt a, Bt b) => Integer -> [a] -> [b] -> Jazz [Bit]
      aux _ [] xs = mapM bit xs
      aux i (w:ws) xs = do
        xs <- aux (2*i) ws xs
        y1 <- conc
                (List.genericReplicate i False)
                (List.genericTake (List.genericLength xs - i) xs)
        y2 <- conc
                (List.genericDrop i xs)
                (List.genericReplicate i False)
        ys <- mux a y1 y2
        bits $ mux w ys xs
  in
  aux 1 ws xs

-- signed and unsigned extension
-- signed n value
extend :: (Bt a, Bt b) => a -> Integer -> [b] -> Jazz [Bit]
extend s n xs = do
  a <- s /\ (List.head $ List.reverse xs)
  bits $ conc xs $ mux a
              (List.genericReplicate n True)
              (List.genericReplicate n False)


-- get_ctrl_alu :: Instr -> Jazz (Alu_control)

-- instr ram_output alu_output alu_flags
-- write_output :: (Bt a, Bt b) => Instr -> [a] -> [b] -> Alu_flag -> Jazz ()

-- instr data addr
-- memory :: (Bt a, Bt b) => Instr -> [a] -> [b] -> Jazz [Bit]

