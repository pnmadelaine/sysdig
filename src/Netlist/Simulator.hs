module Netlist.Simulator (simulate) where

import qualified Data.Map.Strict as Map
import qualified Data.List as List
import Data.Map.Lazy ((!))

import Netlist.Ast

word_size :: Int
word_size = 8

addr_size :: Int
addr_size = 8

def :: Value
def = List.replicate word_size False

int_of_bool_list :: [Bool] -> Integer
int_of_bool_list l = aux 0 (List.reverse l)
  where aux acc []         = acc
        aux acc (True:xs)  = aux (1+2*acc) xs
        aux acc (False:xs) = aux (2*acc)   xs

read_ram :: Value -> Ram -> Value
read_ram addr ram = Map.findWithDefault def (int_of_bool_list addr) ram

write_ram :: Value -> Value -> Ram -> Ram
write_ram addr x ram = Map.insert (int_of_bool_list addr) x ram

value :: Map.Map Ident Value -> Argument -> Value
value m (ArgVar i) = m ! i
value _ (ArgCst c) = c

get_slice :: Integer -> Integer -> [Bool] -> [Bool]
--get_slice 0 0 _      = []
--get_slice 0 j (x:xs) = x:(get_slice 0 (j-1) xs)
--get_slice i j (x:xs) = get_slice (i-1) (j-1) xs
get_slice i j l = List.genericDrop i $ List.genericTake j l

update_vars :: Ram -> Ram -> Vars -> Vars -> Equation -> Vars
update_vars rom ram regs vars (id, exp) =
  let v = case exp of
            Earg a           -> value vars a
            Ereg i           -> regs ! i
            Enot a           -> List.map not (value vars a)
            Ebinop op a b    -> apply_op op (value vars a) (value vars b)
            Emux a b c       -> case value vars a of
                                  [True]  -> value vars b
                                  [False] -> value vars c
            Erom a           -> read_ram (value vars a) rom
            Eram a _ _ _     -> read_ram (value vars a) ram
            Econcat a b      -> (value vars b) ++ (value vars a)
            Eslice i1 i2 a   -> get_slice i1 i2 (value vars a)
            Eselect i a      -> get_slice i (i+1) (value vars a)
  in Map.insert id v vars

update_ram :: Vars -> Ram -> Equation -> Ram
update_ram vars ram (_, Eram _ we wa dt) =
  case value vars we of
    [True]  -> Map.insert (int_of_bool_list (value vars wa)) (value vars dt) ram
    [False] -> ram
update_ram _ ram _ = ram

simulate :: Integer -> Ram -> Ram -> Vars -> Netlist -> IO (Ram, Vars)
simulate 0 _ ram regs net = do
  return (ram,regs)
simulate n rom ram vars net =
  let vars' = List.foldl (update_vars rom ram vars) vars (netlist_eq net) in
  let ram'  = List.foldl (update_ram vars') ram (netlist_eq net) in
  simulate (n-1) rom ram' vars' net

