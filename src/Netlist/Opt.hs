module Netlist.Opt where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Map.Lazy ((!))
import Data.Maybe (fromJust)
import Data.Functor ((<$>))

import Netlist.Ast
import Netlist.Scheduler
import Netlist.Show
import Netlist.Typer -- for debugging
import Netlist.Jazz

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
  let filter id = Set.member id usefull in
  Netmap { netmap_eqs   = Map.filterWithKey (\i e -> filter i) $ netmap_eqs netmap
         , netmap_ids   = Map.filterWithKey (\e i -> filter i) $ netmap_ids netmap
         , netmap_in    = Set.filter filter (netmap_in netmap)
         , netmap_out   = netmap_out netmap
         , netmap_sizes = Map.filterWithKey (\i n -> filter i) $ netmap_sizes netmap
         }

opt_tauto :: Netmap -> Equation -> Netmap
opt_tauto netmap (id, exp) =
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
  add_eq (id, exp') netmap

opt_tauto_1 :: Netmap -> Equation -> Netmap
opt_tauto_1 netmap (id, Econcat (ArgVar id1) (ArgVar id2)) =
  let aux (Eselect i x) = Eslice i (i+1) x
      aux exp = exp
  in
  case (aux <$> Map.lookup id1 (netmap_eqs netmap), aux <$> Map.lookup id2 (netmap_eqs netmap)) of
    (Just (Eslice k l y), Just (Eslice i j x)) ->
      if j == k && x == y then
          add_eq (id, Eslice i l x) netmap
      else
          add_eq (id, Econcat (ArgVar id1) (ArgVar id2)) netmap
    _ -> add_eq (id, Econcat (ArgVar id1) (ArgVar id2)) netmap

opt_tauto_1 netmap eq = add_eq eq netmap

apply_opt :: Netlist -> (Netmap -> Equation -> Netmap) -> Netlist
apply_opt netlist opt =
  let Right net_sch = schedule netlist in
  let netmap0 = Netmap { netmap_eqs   = Map.empty
                       , netmap_ids   = Map.empty
                       , netmap_in    = Set.fromList (netlist_in net_sch)
                       , netmap_out   = Set.fromList (netlist_out net_sch)
                       , netmap_sizes =
                           Map.fromList $ List.map
                           (\id -> (id, (fromJust $ List.lookup id (netlist_var net_sch))))
                           (netlist_out net_sch ++ netlist_in net_sch)
                       } in
  let netmap1 = List.foldl opt netmap0 (netlist_eq net_sch) in
  netlist_from_netmap (clear netmap1)

optimize :: Netlist -> Netlist -- TODO: Netlist -> Either String Netlist
optimize netlist =
  let res = List.foldl apply_opt netlist [opt_tauto, opt_tauto_1] in
  case verify res of
    Left err -> error "optimization failed"
    Right m -> res

