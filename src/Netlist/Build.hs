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

type Jazz = State Env
type Funk = Jazz ()

add_exp :: Expression -> Integer -> Jazz Ident
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

input :: Ident -> Integer -> Jazz Argument
input id n = do s <- get
                let s' = Env { env_ids   = env_ids s
                             , env_in    = Set.insert id (env_in s)
                             , env_out   = env_out s
                             , env_sizes = Map.insert id n (env_sizes s)
                             }
                put s' >> return (ArgVar id)

output :: Ident -> Integer -> Jazz Argument -> Jazz ()
output id n x = do a <- x
                   let exp = Earg a
                   s <- get
                   let s' = Env { env_ids   = Map.insert exp id $ env_ids s
                                , env_in    = env_in s
                                , env_out   = Set.insert id $ env_out s
                                , env_sizes = Map.insert id n $ env_sizes s
                                }
                   put s'

arg_size :: Argument -> Jazz Integer
arg_size (ArgCst v) = return (List.genericLength v)
arg_size (ArgVar id) = do s <- get
                          let n = (env_sizes s) ! id
                          return n

arg :: Argument -> Jazz Argument
arg = return

argvar :: Ident -> Jazz Argument
argvar id = return (ArgVar id)

argcst :: Value -> Jazz Argument
argcst v = return (ArgCst v)

neg :: Jazz Argument -> Jazz Argument
neg x = do a <- x
           let exp = Enot a
           n <- arg_size a
           id <- add_exp exp n
           return (ArgVar id)

reg :: Ident -> Integer -> Jazz Argument -> Jazz Argument
reg id n x = do (ArgVar id') <- x
                let exp = Ereg id'
                s <- get
                let s' = Env { env_ids   = Map.insert exp id $ env_ids s
                             , env_in    = env_in s
                             , env_out   = Set.insert id $ env_out s
                             , env_sizes = Map.insert id n $ env_sizes s
                             }
                put s' >> return (ArgVar id')

binop :: BinOp -> Jazz Argument -> Jazz Argument -> Jazz Argument
binop op x y = do a <- x
                  b <- y
                  let exp = Ebinop op a b
                  n <- arg_size a
                  id <- add_exp exp n
                  return (ArgVar id)

mux :: Jazz Argument -> Jazz Argument -> Jazz Argument -> Jazz Argument
mux x y z = do a <- x
               b <- y
               c <- z
               let exp = Emux a b c
               n <- arg_size b
               id <- add_exp exp n
               return (ArgVar id)

rom :: Jazz Argument -> Jazz Argument
rom x = do a <- x
           let exp = Erom 32 8 a
           let n = 8
           id <- add_exp exp n
           return (ArgVar id)

ram :: Jazz Argument -> Jazz Argument
    -> Jazz Argument -> Jazz Argument -> Jazz Argument
ram ra we wa d = do a1 <- ra
                    a2 <- we
                    a3 <- wa
                    a4 <- d
                    let exp = Eram 32 8 a1 a2 a3 a4
                    let n = 8
                    id <- add_exp exp n
                    return (ArgVar id)

conc :: Jazz Argument -> Jazz Argument -> Jazz Argument
conc x y = do a <- x
              b <- y
              let exp = Econcat a b
              n1 <- arg_size a
              n2 <- arg_size b
              let n = n1 + n2
              id <- add_exp exp n
              return (ArgVar id)

slice :: Integer -> Integer -> Jazz Argument -> Jazz Argument
slice i j x = do a <- x
                 let exp = Eslice i j a
                 let n = j-i
                 id <- add_exp exp n
                 return (ArgVar id)

select :: Integer -> Jazz Argument -> Jazz Argument
select i x = do a <- x
                let exp = Eselect i a
                let n = 1
                id <- add_exp exp n
                return (ArgVar id)

build :: Funk -> Netlist
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

(\/) :: Jazz Argument -> Jazz Argument -> Jazz Argument
x \/ y = binop Or x y

(/\) :: Jazz Argument -> Jazz Argument -> Jazz Argument
x /\ y = binop And x y

(<>) :: Jazz Argument -> Jazz Argument -> Jazz Argument
x <> y = binop Xor x y

funnel :: [Argument] -> Jazz Argument
funnel l = let x:xs = List.map arg l in
           List.foldl (flip conc) x xs

smash :: Argument -> Integer -> Jazz [Argument]
smash x n =
  let aux acc _ 0 = return acc
      aux acc x i = do y <- select (i-1) x
                       aux (y:acc) x (i-1)
  in
  aux [] (arg x) n

squeeze :: [Argument] -> Jazz [Argument]
squeeze xs = do let n = List.genericLength xs
                y <- funnel xs
                smash y n

