module Netlist.Ast where

import qualified Data.Map.Strict as Map
import qualified Data.List as List
import qualified Data.Set as Set
import Data.Map.Lazy ((!))

word_size :: Integer
word_size = 8
addr_size :: Integer
addr_size = 32

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


-- NETMAP

data Netmap = Netmap { netmap_eqs :: Map.Map Ident Expression
                     , netmap_ids :: Map.Map Expression Ident
                     , netmap_in  :: Set.Set Ident
                     , netmap_out :: Set.Set Ident
                     , netmap_sizes :: Map.Map Ident Integer
                     }

arg_size :: Argument -> Netmap -> Integer
arg_size (ArgVar id) netmap = (netmap_sizes netmap) ! id
arg_size (ArgCst v) _ = List.genericLength v

exp_size :: Expression -> Netmap -> Integer
exp_size exp netmap =
  case exp of
    Earg a        -> arg_size a netmap
    Ereg id       -> arg_size (ArgVar id) netmap
    Enot a        -> arg_size a netmap
    Ebinop op a b -> arg_size a netmap
    Emux a b c    -> arg_size b netmap
    Erom a        -> word_size
    Eram a b c d  -> word_size
    Econcat a b   -> arg_size a netmap + arg_size b netmap
    Eslice i j a  -> j - i
    Eselect i a   -> 1

netmap_empty = Netmap { netmap_eqs = Map.empty
                      , netmap_ids = Map.empty
                      , netmap_in = Set.empty
                      , netmap_out = Set.empty
                      , netmap_sizes = Map.empty
                      }

netlist_from_netmap x =
  Netlist { netlist_eq = Map.toAscList $ netmap_eqs x
          , netlist_in = Set.toList $ netmap_in x
          , netlist_out = Set.toList $ netmap_out x
          , netlist_var = Map.toAscList $ netmap_sizes x
          }

netmap_from_netlist x =
  Netmap { netmap_eqs = Map.fromList $ netlist_eq x
         , netmap_ids = Map.fromList $ List.map (\(i,e) -> (e,i)) (netlist_eq x)
         , netmap_in = Set.fromList $ netlist_in x
         , netmap_out = Set.fromList $ netlist_out x
         , netmap_sizes = Map.fromList $ netlist_var x
         }

