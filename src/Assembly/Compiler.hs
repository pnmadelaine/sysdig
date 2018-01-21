module Assembly.Compiler where

import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map ((!))

import Assembly.Ast
import Assembly.Parser

---                  ---
--- CALCUL DES JUMPS ---
---                  ---

get_labels :: Int -> Prog -> Map.Map String Int
get_labels n [] = Map.empty
get_labels n ((Lexpr l):xs) = Map.insert l n (get_labels n xs)
get_labels n (_:xs) = get_labels (n+1) xs

update_jumps :: Map.Map String Int -> Int -> Prog -> Prog
update_jumps m n []                    = []
update_jumps m n ((Jexpr opc l):p)     = let j = convert_imm 26 (m ! l) in
                                         (Jump opc j) : (update_jumps m (n+1) p)
update_jumps m n ((Lexpr l):p)         = update_jumps m n p --on a plus besoin des labels
update_jumps m n ((Bexpr opc s t l):p) = let j = convert_imm 16 ((m ! l) - n -1) in
                                         -- attention PC = PC(n) + 1 + addr
                                         (Iexpr opc s t j) : (update_jumps m (n+1) p)
update_jumps m n (i:p)                 = i : (update_jumps m (n+1) p)

understand_assembly :: Prog -> Prog
understand_assembly p = do let m = get_labels 0 p
                           update_jumps m 0 p


print_tab :: [Bool] -> String
print_tab l = List.reverse . List.map aux $ l
    where aux True = '1'
          aux False = '0'

instance Show Instr where
  show i = bar 0 . foo $ i
      where foo (Rexpr opcode rs rt rd shamt funct) =
              List.concat . List.map print_tab $ [opcode, rs, rt, rd, shamt, funct]
            foo (Iexpr opcode rs rt imm) =
              List.concat . List.map print_tab $ [opcode, rs, rt, imm]
            foo (Jump opcode address) =
              List.concat . List.map print_tab $ [opcode, address]
            foo _ = error "wrong instruction"

            bar _ [] = []
            bar 8 l  = ' ':(bar 0 l)
            bar i (x:xs) = x:(bar (i+1) xs)

print_prog :: Prog -> String
print_prog p = List.unlines . List.map show . List.reverse $ p
