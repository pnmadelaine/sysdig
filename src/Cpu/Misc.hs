module Cpu.Misc where

import Netlist.Jazz

import Control.Monad (mapM, mapM_)
import Data.List as List

nonZero :: Wr a => a -> Jazz Bit
nonZero w =
  let aux :: Bt a => [a] -> Jazz Bit
      aux [x] = bit x
      aux (x:xs) = x \/ nonZero xs
  in
  bits w >>= aux

isZero :: Wr a => a -> Jazz Bit
isZero x = nonZero x >>= neg

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

-- direction = 1 for left shift
shift :: (Bt a, Bt b, Wr c, Wr d) => a -> b -> c -> d -> Jazz Wire
shift dir arith sh x =
  let aux :: Integer -> Wire -> [Bit] -> Jazz Wire
      aux _ w [] = return w
      aux i w (x:xs) = do
        let t = List.genericReplicate i True
        let f = List.genericReplicate i False
        w' <- aux (2*i) w xs
        n  <- wire_size w
        y1 <- conc f (slice 0 (n-i) w)
        y2 <- conc (slice i n w) (mux (select (n-1) w /\ arith) t f)
        w'' <- mux dir y1 y2
        mux x w'' w'
  in
  do w <- wire x
     xs <- bits sh
     aux 1 w xs


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

