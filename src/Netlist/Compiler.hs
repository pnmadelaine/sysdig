module Netlist.Compiler (compile) where

import qualified Data.List as List

import Netlist.Ast

int_of_bool_list :: [Bool] -> Integer
int_of_bool_list l = aux 0 (List.reverse l)
  where aux acc []         = acc
        aux acc (True:xs)  = aux (1+2*acc) xs
        aux acc (False:xs) = aux (2*acc)   xs

c_arg (ArgVar id) = id
c_arg (ArgCst v)  = show $ int_of_bool_list v

compile :: Netlist -> String
compile _ = ""

