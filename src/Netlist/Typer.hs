module Netlist.Typer (verify_netlist) where

import Netlist.Ast
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad (foldM, mapM_)
import Data.Map.Lazy ((!))

check_equation :: (Map.Map Ident Integer) -> Equation -> Bool
check_equation sizes (id, exp) =
  let size_arg a = case a of
                   ArgCst v -> fromIntegral (List.length v)
                   ArgVar i -> sizes ! i
  in
  let n = sizes ! id in
  case exp of
    Earg a           -> n == size_arg a
    Ereg r           -> n == sizes ! r
    Enot a           -> n == size_arg a
    Ebinop op a b    -> n == size_arg a && n == size_arg b
    Emux a b c       -> 1 == size_arg a 
                     && n == size_arg b && n == size_arg b
    Erom i j a       -> i == size_arg a && j == n
    Eram i j a b c d -> i == size_arg a && i == size_arg c
                     && 1 == size_arg b && j == size_arg d
                     && j == n
    Econcat a b      -> n == size_arg a + size_arg b
    Eslice i j a     -> j >= size_arg a && n == j-i
    Eselect i a      -> i < size_arg a

aux_eqs sizes ins eqs (id,exp) =
  if Map.member id eqs then
    Left $ "\"" ++ id ++ "\" is defined by more than one equation"
  else if Set.member id ins then
    Left $ "\"" ++ id ++ "\" is defined by an equation although it is an input"
  else if not (List.all (\i -> Map.member i sizes) (get_idents exp)) then
    Left $ "Undefined variable in the equation of \"" ++ id ++ "\""
  else if not (check_equation sizes (id,exp)) then
    Left $ "Error in the equation of \"" ++ id ++ "\""
  else
    Right $ Map.insert id exp eqs
    
aux_vars vars (id,n) =
  if Map.member id vars then
    Left $ "\"" ++ id ++ "\" is defined several times in VAR"
  else
    Right $ Map.insert id n vars

aux_ins vars ins id =
  if not (Map.member id vars) then
    Left $ "\"" ++ id ++ "\" is not defined in VAR"
  else if Set.member id ins then
    Left $ "\"" ++ id ++ "\" is appears several times in IN"
  else
    Right $ Set.insert id ins

aux_outs vars outs id =
  if not (Map.member id vars) then
    Left $ "\"" ++ id ++ "\" is not defined in VAR"
  else if Set.member id outs then
    Left $ "\"" ++ id ++ "\" is appears several times in OUT"
  else
    Right $ Set.insert id outs

aux_args eqs ins (id,_) =
  if Set.member id ins || Map.member id eqs then
    Right ()
  else
    Left $ "\"" ++ id ++ "\" must be defined by an equation"

verify_netlist :: Netlist -> Either String (Map.Map Ident Expression)
verify_netlist net = do
  sizes <- foldM aux_vars            Map.empty (var net)
  ins   <- foldM (aux_ins sizes)     Set.empty (input net)
  outs  <- foldM (aux_outs sizes)    Set.empty (output net)
  eqs   <- foldM (aux_eqs sizes ins) Map.empty (equations net)
  mapM_ (aux_args eqs ins) (var net) 
  return eqs
  
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

