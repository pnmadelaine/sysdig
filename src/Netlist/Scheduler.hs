module Netlist.Scheduler (schedule) where

import Netlist.Ast
import Graph

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)

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
  Erom _ _ (ArgVar i)       -> i:foo
  Econcat (ArgVar i) _      -> i:foo
  Eslice _ _ (ArgVar i)     -> i:foo
  Eselect _(ArgVar i)       -> i:foo
  Ebinop _ (ArgVar i) _     -> i:foo
  Emux (ArgVar i) _ _       -> i:foo
  Eram _ _ (ArgVar i) _ _ _ -> i:foo
  _                         -> foo
                           
make_graph :: Netlist -> Graph.Graph Ident
make_graph net =
  let graph0 = List.foldl (\g (i,_) -> Graph.add_node i g)
                          Graph.empty
                          (var net) 
  in
  let aux graph (i,exp) = List.foldl (\g j -> add_edge i j g)
                          graph
                          (get_idents exp)
  in
  List.foldl aux graph0 (equations net)

schedule :: Netlist -> Either String Netlist
schedule net = do
  let graph = make_graph net
  ord <- topological graph
  let eqs = Map.fromList (equations net) 
  let get_eq i = Map.lookup i eqs >>= (\eq -> return (i,eq))
  let l = List.map get_eq ord
  let new_equations = List.reverse $ catMaybes l
  return $ Netlist { equations = new_equations
                   , input     = (input net)
                   , output    = (output net)
                   , var       = (var net)
                   }

