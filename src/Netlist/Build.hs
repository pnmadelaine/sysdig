module Netlist.Build where

import qualified Data.Map.Strict as Map
import qualified Data.List as List
import qualified Data.Set as Set
import Data.Map.Lazy ((!))

import Netlist.Ast
import Netlist.Show
import Control.Monad.State.Lazy

data Env = Env { env_ids   :: Map.Map Expression Ident
               , env_in    :: Set.Set Ident
               , env_out   :: Set.Set Ident
               , env_sizes :: Map.Map Ident Integer
               }

add_exp :: Expression -> Integer -> State Env Ident
add_exp exp n = do s <- get
                   case Map.lookup exp (env_ids s) of
                     Just id -> return id
                     Nothing -> let id = "_x" ++ show (Map.size (env_ids s)) in
                                let s' = Env { env_ids   = Map.insert exp id $ env_ids s
                                             , env_in    = env_in s
                                             , env_out   = env_out s
                                             , env_sizes = Map.insert id n $ env_sizes s
                                             }
                                in
                                put s' >> return id

input :: Ident -> Integer -> State Env Argument
input id n = do s <- get
                let s' = Env { env_ids   = env_ids s
                             , env_in    = Set.insert id (env_in s)
                             , env_out   = env_out s
                             , env_sizes = Map.insert id n (env_sizes s)
                             }
                put s' >> return (ArgVar id)

output :: Ident -> Integer -> State Env Argument -> State Env ()
output id n x = do a <- x
                   let exp = Earg a
                   s <- get
                   let s' = Env { env_ids   = Map.insert exp id $ env_ids s
                                , env_in    = env_in s
                                , env_out   = Set.insert id $ env_out s
                                , env_sizes = Map.insert id n $ env_sizes s
                                }
                   put s'

arg_size :: Argument -> State Env Integer
arg_size (ArgCst v) = return (List.genericLength v)
arg_size (ArgVar id) = do s <- get
                          let n = (env_sizes s) ! id
                          return n

argvar :: Ident -> State Env Argument
argvar id = return (ArgVar id)

argcst :: Value -> State Env Argument
argcst v = return (ArgCst v)

neg :: State Env Argument -> State Env Argument
neg x = do a <- x
           let exp = Enot a
           n <- arg_size a
           id <- add_exp exp n
           return (ArgVar id)

reg :: Ident -> State Env Argument
reg id = do let exp = Ereg id
            n <- arg_size (ArgVar id)
            id <- add_exp exp n
            return (ArgVar id)

binop :: BinOp -> State Env Argument -> State Env Argument -> State Env Argument
binop op x y = do a <- x
                  b <- y
                  let exp = Ebinop op a b
                  n <- arg_size a
                  id <- add_exp exp n
                  return (ArgVar id)

mux :: State Env Argument -> State Env Argument -> State Env Argument -> State Env Argument
mux x y z = do a <- x
               b <- y
               c <- z
               let exp = Emux a b c
               n <- arg_size b
               id <- add_exp exp n
               return (ArgVar id)

rom :: State Env Argument -> State Env Argument
rom x = do a <- x
           let exp = Erom 32 8 a
           let n = 8
           id <- add_exp exp n
           return (ArgVar id)

ram :: State Env Argument -> State Env Argument -> State Env Argument -> State Env Argument -> State Env Argument
ram ra we wa d = do a1 <- ra
                    a2 <- we
                    a3 <- wa
                    a4 <- d
                    let exp = Eram 32 8 a1 a2 a3 a4
                    let n = 8
                    id <- add_exp exp n
                    return (ArgVar id)

conc :: State Env Argument -> State Env Argument -> State Env Argument
conc x y = do a <- x
              b <- y
              let exp = Econcat a b
              n1 <- arg_size a
              n2 <- arg_size b
              let n = n1 + n2
              id <- add_exp exp n
              return (ArgVar id)

slice :: Integer -> Integer -> State Env Argument -> State Env Argument
slice i j x = do a <- x
                 let exp = Eslice i j a
                 let n = j-i
                 id <- add_exp exp n
                 return (ArgVar id)

select :: Integer -> State Env Argument -> State Env Argument
select i x = do a <- x
                let exp = Eselect i a
                let n = 1
                id <- add_exp exp n
                return (ArgVar id)

build :: State Env () -> Netlist
build x =
  let env_empty = Env { env_ids   = Map.empty
                      , env_in    = Set.empty
                      , env_out   = Set.empty
                      , env_sizes = Map.empty
                      }
  in
  let (_,s) = runState x env_empty in
  let ids = Map.toAscList (env_ids s) in
  let eqs = List.map (\(e,i) -> (i,e)) ids in
  let input = Set.toList (env_in s) in
  let output = Set.toList (env_out s) in
  let var = List.map (\(_,i) -> (i, (env_sizes s) ! i)) ids
         ++ List.map (\i -> (i, (env_sizes s) ! i)) input
  in Netlist { netlist_eq  = eqs
             , netlist_var = var
             , netlist_in  = input
             , netlist_out = output
             }

(\/) :: State Env Argument -> State Env Argument -> State Env Argument
x \/ y = binop Or x y

(/\) :: State Env Argument -> State Env Argument -> State Env Argument
x /\ y = binop And x y

(<>) :: State Env Argument -> State Env Argument -> State Env Argument
x <> y = binop Xor x y

funnel (x:xs) = List.foldl conc x xs

smash xs n = List.map (\i -> select i xs) [0..n-1]

squeeze xs = let n = List.genericLength xs in
             let x = funnel xs in
             smash x n
