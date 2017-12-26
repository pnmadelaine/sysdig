module Netlist.Opt where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Netlist.Ast
import Netlist.Scheduler
import Netlist.Show

fixpoint f x = if x == f x then x else fixpoint f (f x)

-- delete useless equations and wires in a netmap
clear :: Netmap -> Netmap
clear netmap =
  let exps = netmap_eqs netmap in
  let f :: Set.Set Ident -> Set.Set Ident
      f s =
        let aux id =
              case Map.lookup id exps of
                Nothing         -> Set.empty
                Just (Ereg id') -> Set.singleton id'
                Just exp        -> Set.fromList (get_idents exp)
        in
        Set.unions $ [s, netmap_out netmap] ++ (Set.toList (Set.map aux s))
  in
  let usefull = fixpoint f Set.empty in
  let filter id _ = Set.member id usefull in
  Netmap { netmap_eqs = Map.filterWithKey filter $ netmap_eqs netmap
         , netmap_ids = Map.filterWithKey (flip filter) $ netmap_ids netmap
         , netmap_in = netmap_in netmap
         , netmap_out = netmap_out netmap
         , netmap_sizes = Map.filterWithKey filter $ netmap_sizes netmap
         }

opt_triv :: Netmap -> Equation -> Netmap
opt_triv netmap (id, exp) =
  let exps = netmap_eqs netmap in
  let aux :: Argument -> Argument
      aux (ArgVar id) = case Map.lookup id exps of
                          Just (Earg a) -> a -- assert: a == ArgCst v 
                          _             -> ArgVar id
      aux (ArgCst v) = ArgCst v
  in
  let exp' = case exp of
        Earg (ArgVar id) -> case Map.lookup id exps of
                              Just exp' -> exp'
                              Nothing   -> Earg (ArgVar id)
        Earg (ArgCst v)  -> Earg (ArgCst v)
        Ereg id          -> Ereg id
        Enot a           -> case aux a of
                              ArgCst v -> Earg $ ArgCst (List.map not v)
                              a'       -> Enot a'
        Ebinop op a b    -> case (aux a, aux b) of
                              (ArgCst u, ArgCst v) -> Earg $ ArgCst (apply_op op u v)
                              (a', b')             -> Ebinop op a' b'
        Emux a b c       -> case aux a of
                              ArgCst [x] -> if x then Earg $ aux b else Earg $ aux c
                              a'         -> Emux a' (aux b) (aux c)
        Erom a           -> Erom $ aux a
        Eram ra we wa dt -> Eram (aux ra) (aux we) (aux wa) (aux dt)
        Econcat a b      -> case (aux a, aux b) of
                              (ArgCst u, ArgCst v) -> Earg $ ArgCst (v ++ u)
                              (a', b')             -> Econcat a' b'
        Eslice i j a     -> case aux a of
                              ArgCst u -> Earg $ ArgCst (List.genericTake (j-i)
                                               $ List.genericDrop i u)
                              a'       -> Eslice i j a'
        Eselect i a      -> case aux a of
                              ArgCst u -> Earg $ ArgCst (List.genericTake 1 $ List.genericDrop i u)
                              a'       -> Eselect i a'
  in
  Netmap { netmap_eqs   = Map.insert id exp' $ netmap_eqs netmap
         , netmap_ids   = Map.insert exp' id $ netmap_ids netmap
         , netmap_in    = netmap_in netmap
         , netmap_out   = netmap_out netmap
         , netmap_sizes = netmap_sizes netmap
         }

optimize :: Netlist -> Netlist
optimize net =
  let Right net_sch = schedule net in
  let netmap0 = netmap_from_netlist net_sch in
  let netmap1 = List.foldl opt_triv netmap0 (netlist_eq net_sch) in
  let netmap2 = clear netmap1 in
  netlist_from_netmap netmap2

