module Netlist.Opt where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Map.Lazy ((!))

import Netlist.Ast
import Netlist.Scheduler
import Netlist.Show

import Debug.Trace

clear :: Netmap -> Netmap
clear netmap =
  let exps = netmap_eqs netmap in
  let f :: Expression -> Set.Set Ident
      f (Ereg id) = Set.singleton id
      f exp       = Set.fromList $ get_idents exp
  in
  let g :: Set.Set Ident -> Ident -> Set.Set Ident
      g s id = case Map.lookup id exps of
                 Nothing  -> s
                 Just exp -> Set.foldl (flip Set.insert) s (f exp)
  in
  let aux :: Set.Set Ident -> Set.Set Ident
      aux s =
        let s' = Set.foldl g s s
        in
        if s' Set.\\ s == Set.empty then s'
                                    else aux s'
  in
  let usefull = aux (netmap_out netmap)
  in
  let h id _ = Set.member id usefull
  in
  Netmap { netmap_eqs = Map.filterWithKey h $ netmap_eqs netmap
         , netmap_ids = Map.filterWithKey (flip h) $ netmap_ids netmap
         , netmap_in = netmap_in netmap
         , netmap_out = netmap_out netmap
         , netmap_sizes = Map.filterWithKey h $ netmap_sizes netmap
         }



opt_const :: Netmap -> Equation -> Netmap
opt_const netmap (id, exp) =
  let exps = netmap_eqs netmap in
  let aux :: Argument -> Argument
      aux (ArgVar id) = case Map.lookup id exps of
                          Just (Earg a) -> a
                          _             -> ArgVar id
      aux (ArgCst v) = ArgCst v
  in
  let exp' =
       case exp of
         Earg (ArgVar id) -> case Map.lookup id exps of
                               Just exp' -> exp'
                               Nothing   -> Earg (ArgVar id)
         Earg (ArgCst v) -> Earg (ArgCst v)
         Ereg id -> Ereg id
         Enot a -> case aux a of
                     ArgCst v -> Earg $ ArgCst (List.map not v)
                     a'       -> Earg a'
         Ebinop op a b -> case (aux a, aux b) of
                            (ArgCst u, ArgCst v) -> Earg $ ArgCst (apply_op op u v)
                            (a', b')             -> Ebinop op a' b'
         Emux a b c -> case aux a of
                         ArgCst [x] -> if x then Earg $ aux b else Earg $ aux c
                         a'         -> Emux a' (aux b) (aux c)
         Erom a -> Erom $ aux a
         Eram ra we wa dt -> Eram (aux ra) (aux we) (aux wa) (aux dt)
         Econcat a b -> case (aux a, aux b) of
                          (ArgCst u, ArgCst v) -> Earg $ ArgCst (v ++ u)
                          (a', b')             -> Econcat a' b'
         Eslice i j a -> case aux a of
                           ArgCst u -> Earg $ ArgCst (List.genericTake (j-i) $ List.genericDrop i u)
                           a'       -> Eslice i j a'
         Eselect i a -> case aux a of
                           ArgCst u -> Earg $ ArgCst (List.genericTake 1 $ List.genericDrop i u)
                           a'       -> Eselect i a'
  in
  Netmap { netmap_eqs = Map.insert id exp' $ netmap_eqs netmap
         , netmap_ids = Map.insert exp' id $ netmap_ids netmap
         , netmap_in = netmap_in netmap
         , netmap_out = netmap_out netmap
         , netmap_sizes = netmap_sizes netmap
         }

optimize :: Netlist -> Netlist
optimize net =
  let Right net_sch = schedule net in
  let m = netmap_from_netlist net_sch in
  let m' = List.foldl opt_const m (netlist_eq net_sch) in
  netlist_from_netmap (clear m')

