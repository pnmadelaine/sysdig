module Netlist.Build where

import qualified Data.Map.Strict as Map
import qualified Data.List as List
import qualified Data.Set as Set
import Data.Map.Lazy ((!))
import Data.Tuple (fst)
import Data.Maybe (fromJust)

import Netlist.Ast
import Netlist.Show

data Formula = Fcst Value
             | Fnot Formula
             | Fop BinOp Formula Formula
             | Fselect Integer Wire
             deriving (Eq, Ord, Show)

data Wire = Wbit Formula
          | Winput Ident
          | Wreg Ident
          | Wmux Formula Wire Wire
          | Wrom Wire
          | Wram Wire Formula Wire Wire
          | Wconcat Wire Wire
          | Wslice Integer Integer Wire
          deriving (Eq, Ord, Show)

(\/) :: Formula -> Formula -> Formula
a \/ b = Fop Or a b

(/\) :: Formula -> Formula -> Formula
a /\ b = Fop And a b

xor :: Formula -> Formula -> Formula
xor a b = Fop Xor a b

nand :: Formula -> Formula -> Formula
nand a b = Fop Nand a b

data Netmap = Netmap { n_form :: Map.Map Formula Ident
                     , n_wire :: Map.Map Wire Ident
                     , n_eq   :: Map.Map Ident Expression
                     , n_size :: Map.Map Ident Integer
                     }

add_wire_to_netmap :: Wire -> Netmap -> Netmap
add_wire_to_netmap w net =
  case Map.lookup w (n_wire net) of
    Just id -> net
    Nothing ->
      let net' = case w of
           Wbit a          -> add_formula_to_netmap a net
           Winput id       -> net
           Wreg id         -> net
           Wmux a w1 w2    -> add_formula_to_netmap a
                              $ add_wire_to_netmap w1
                              $ add_wire_to_netmap w2 net
           Wrom w          -> add_wire_to_netmap w net
           Wram w1 a w2 w3 -> add_formula_to_netmap a
                              $ add_wire_to_netmap w1
                              $ add_wire_to_netmap w2
                              $ add_wire_to_netmap w3 net
           Wconcat w1 w2   -> add_wire_to_netmap w1
                              $ add_wire_to_netmap w2 net
           Wslice i1 i2 w  -> add_wire_to_netmap w net
      in
      let (size, exp) = case w of
           Wbit a          -> let id = n_form net' ! a in
                              ( 1
                              , Earg (ArgVar id)
                              )
           Winput id       -> ( n_size net' ! id
                              , Earg (ArgVar id)
                              )
           Wreg id         -> ( n_size net' ! id
                              , Ereg id
                              )
           Wmux a w1 w2    -> let id  = n_form net' ! a in
                              let id1 = n_wire net' ! w1 in
                              let id2 = n_wire net' ! w2 in
                              ( n_size net' ! id1
                              , Emux (ArgVar id) (ArgVar id1) (ArgVar id2)
                              )
           Wrom w          -> let id = n_wire net' ! w in
                              ( 8, Erom 32 8 (ArgVar id) )
           Wram w1 a w2 w3 -> let id  = n_form net' ! a in
                              let id1 = n_wire net' ! w1 in
                              let id2 = n_wire net' ! w2 in
                              let id3 = n_wire net' ! w3 in
                              ( 8
                              , Eram 32 8 (ArgVar id1) (ArgVar id)
                                          (ArgVar id2) (ArgVar id3)
                              )
           Wconcat w1 w2   -> let id1 = n_wire net' ! w1 in
                              let id2 = n_wire net' ! w2 in
                              ( n_size net' ! id1 + n_size net' ! id2
                              , Econcat (ArgVar id1) (ArgVar id2)
                              )
           Wslice i1 i2 w  -> let id = n_wire net' ! w in
                              ( i2 - i1
                              , Eslice i1 i2 (ArgVar id)
                              )
      in
      let id' = "_b" ++ show (Map.size (n_wire net'))
      in
      Netmap { n_form = n_form net'
             , n_wire = Map.insert w id' (n_wire net')
             , n_eq   = Map.insert id' exp (n_eq net')
             , n_size = Map.insert id' size (n_size net')
             }
            
add_formula_to_netmap :: Formula -> Netmap -> Netmap
add_formula_to_netmap a net =
  case Map.lookup a (n_form net) of
    Just id -> net
    Nothing ->
      let net' = case a of
           Fcst v      -> net
           Fnot a      -> add_formula_to_netmap a net
           Fop op a b  -> add_formula_to_netmap a
                          $ add_formula_to_netmap b net
           Fselect i w -> add_wire_to_netmap w net
      in
      let exp = case a of
           Fcst v      -> Earg (ArgCst v)
           Fnot a      -> let id = n_form net' ! a in
                          Enot (ArgVar id)
           Fop op a b  -> let id1 = n_form net' ! a in
                          let id2 = n_form net' ! b in
                          Ebinop op (ArgVar id1) (ArgVar id2)
           Fselect i w -> let id = n_wire net' ! w in
                          Eselect i (ArgVar id)
      in
      let id' = "_i" ++ show (Map.size (n_form net'))
      in
      Netmap { n_form = Map.insert a id' (n_form net')
             , n_wire = n_wire net'
             , n_eq   = Map.insert id' exp (n_eq net')
             , n_size = n_size net'
             }

add_def_to_netmap :: (Ident, Wire) -> Netmap -> Netmap
add_def_to_netmap (id, w) net =
  let net' = add_wire_to_netmap w net in
  let id'  = n_wire net' ! w in
  let exp  = Earg (ArgVar id') in
  let size = n_size  net' ! id' in
  Netmap { n_form = n_form net'
         , n_wire = n_wire net'
         , n_eq   = Map.insert id exp (n_eq net')
         , n_size = Map.insert id size (n_size net')
         }

build :: [(Ident, Integer)] -> [(Ident, Integer)] -> [(Ident, Wire)] -> Netlist
build inputs outputs defs =
  let init_size = List.foldl (\m (i,n) -> Map.insert i n m)
                             Map.empty
                             (inputs ++ outputs)
  in
  let netmap = Netmap { n_form = Map.empty
                      , n_wire = Map.empty
                      , n_eq   = Map.empty
                      , n_size = init_size
                      }
  in
  let netmap' = List.foldl (flip add_def_to_netmap) netmap defs
  in
  let var_map = Map.foldlWithKey (\m _ i -> Map.insert i 1 m)
                                (n_size netmap') 
                                (n_form netmap')
  in
  Netlist { equations = Map.toAscList (n_eq netmap')
          , input     = List.map fst inputs
          , output    = List.map fst outputs
          , var       = Map.toAscList var_map
          }

--

funnel :: [Formula] -> Wire
funnel l =
  let k = List.map Wbit l in
  List.foldl Wconcat (List.head k) (List.tail k)
 
smash :: Wire -> Integer -> [Formula]
smash w n =
  List.map (\i -> Fselect (n-i) w) [1..n]

squeeze :: [Formula] -> [Formula]
squeeze l =
  let n = List.genericLength l in
  let w = funnel l in
  smash w n

