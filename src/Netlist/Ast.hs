module Netlist.Ast where

import qualified Data.Map.Strict as Map
import qualified Data.List as List

type Ident = String
type Value = [Bool]

data BinOp = Or | Xor | And | Nand
             deriving (Eq, Ord)

data Argument = ArgCst Value | ArgVar Ident
                deriving (Eq, Ord)

data Expression = Earg Argument
                | Ereg Ident
                | Enot Argument
                | Ebinop BinOp Argument Argument
                | Emux Argument Argument Argument
                | Erom Argument
                  -- read_addr
                | Eram Argument Argument Argument Argument
                  -- read_addr write_enable write_addr data
                | Econcat Argument Argument
                | Eslice Integer Integer Argument
                | Eselect Integer Argument
                deriving (Eq, Ord)

type Equation = (Ident, Expression)

data Netlist = Netlist { netlist_eq  :: [Equation]
                       , netlist_in  :: [Ident]
                       , netlist_out :: [Ident]
                       , netlist_var :: [(Ident,Integer)]
                       }

type Ram  = Map.Map Integer Value
type Vars = Map.Map Ident Value

empty_netlist = Netlist { netlist_eq  = []
                        , netlist_in  = []
                        , netlist_out = []
                        , netlist_var = []
                        }

apply_op :: BinOp -> [Bool] -> [Bool] -> [Bool]
apply_op op a b =
  let f = case op of
            Or   -> \(x,y) -> x || y
            Xor  -> \(x,y) -> x /= y
            And  -> \(x,y) -> x && y
            Nand -> \(x,y) -> not(x&&y)
  in
  List.map f (List.zip a b)

get_idents :: Expression -> [Ident]
get_idents expr =
  let bar = case expr of
            Emux _ _ (ArgVar i) -> [i]
            _                   -> []
  in
  let foo = case expr of
            Ebinop _ _ (ArgVar i) -> i:bar
            Emux _ (ArgVar i) _   -> i:bar
            Econcat _ (ArgVar i)  -> i:bar
            _                     -> bar
  in
  case expr of
  Earg (ArgVar i)           -> i:foo
  Enot (ArgVar i)           -> i:foo
  Erom (ArgVar i)           -> i:foo
  Econcat (ArgVar i) _      -> i:foo
  Eslice _ _ (ArgVar i)     -> i:foo
  Eselect _(ArgVar i)       -> i:foo
  Ebinop _ (ArgVar i) _     -> i:foo
  Emux (ArgVar i) _ _       -> i:foo
  Eram (ArgVar i) _ _ _     -> i:foo
  _                         -> foo

