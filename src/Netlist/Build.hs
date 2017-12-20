{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Netlist.Build where

import qualified Data.Map.Strict as Map
import qualified Data.List as List
import qualified Data.Set as Set
import Data.Map.Lazy ((!))

import Data.Functor.Identity (Identity)

import Netlist.Ast
import Netlist.Show
import Control.Monad.State.Lazy

addr_size = 8
word_size = 8

data Env = Env { env_ids   :: Map.Map Expression Ident
               , env_in    :: Set.Set Ident
               , env_out   :: Set.Set Ident
               , env_sizes :: Map.Map Ident Integer
               }

type Jazz = State Env

class Bit a where
  funk :: a -> Jazz Argument
class Wire a where
  prog :: a -> Jazz [Argument]

instance Bit a => Wire [a] where
  prog l = mapM funk l

instance Wire a => Wire (Jazz a) where
  prog x = do
    l <- x
    prog l

instance Bit Argument where
  funk x = return x

instance Bit (Jazz Argument) where
  funk x = x

instance Bit Bool where
  funk x = return $ ArgCst [x]

instance Bit Integer where
  funk 0 = return $ ArgCst [False]
  funk 1 = return $ ArgCst [True]

add_exp :: Expression -> Integer -> Jazz Ident
add_exp exp n =
  get >>= \s ->
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

arg_size :: Argument -> Jazz Integer
arg_size (ArgCst v) = return (List.genericLength v)
arg_size (ArgVar id) = do s <- get
                          let n = (env_sizes s) ! id
                          return n

input :: Ident -> Integer -> Jazz [Argument]
input id n = do s <- get
                let s' = Env { env_ids   = env_ids s
                             , env_in    = Set.insert id (env_in s)
                             , env_out   = env_out s
                             , env_sizes = Map.insert id n (env_sizes s)
                             }
                put s' >> smash (n, ArgVar id)

output :: Wire a => Ident -> a -> Jazz ()
output id l = do w <- prog l
                 (n, a) <- funnel w
                 let exp = Earg a
                 s <- get
                 let s' = Env { env_ids   = Map.insert exp id $ env_ids s
                              , env_in    = env_in s
                              , env_out   = Set.insert id $ env_out s
                              , env_sizes = Map.insert id n $ env_sizes s
                              }
                 put s'

read_reg :: Ident -> Integer -> Jazz [Argument]
read_reg id n = smash (n, ArgVar id)

write_reg :: Wire a => Ident -> a -> Jazz ()
write_reg id x = do (n, ArgVar id') <- prog x >>= funnel
                    let exp = Ereg id'
                    s <- get
                    let s' = Env { env_ids   = Map.insert exp id (env_ids s)
                                 , env_in    = env_in s
                                 , env_out   = Set.insert id (env_out s)
                                 , env_sizes = Map.insert id n (env_sizes s)
                                 }
                    put s'

neg :: Bit a => a -> Jazz Argument
neg x = do a <- funk x
           let exp = Enot a
           n <- arg_size a
           id <- add_exp exp n
           return (ArgVar id)

binop :: (Bit a, Bit b) => BinOp -> a -> b -> Jazz Argument
binop op x y = do a <- funk x
                  b <- funk y
                  let exp = Ebinop op a b
                  n <- arg_size a
                  id <- add_exp exp n
                  return (ArgVar id)

mux :: (Bit a, Wire b, Wire c) => a -> b -> c -> Jazz [Argument]
mux a xs ys = do a <- funk a
                 (n, x) <- prog xs >>= funnel
                 (_, y) <- prog ys >>= funnel
                 let exp = Emux a x y
                 id <- add_exp exp n
                 smash (n, (ArgVar id))

rom :: Wire a => a -> Jazz [Argument]
rom xs = do (_, a) <- prog xs >>= funnel
            let exp = Erom a
            id <- add_exp exp word_size
            smash (word_size, ArgVar id)

ram :: (Wire a, Bit b, Wire c, Wire d) => a -> b -> c -> d -> Jazz [Argument]
ram ra we wa d = do (_, a) <- prog ra >>= funnel
                    b <- funk we
                    (_, c) <- prog wa >>= funnel
                    (_, d) <- prog d  >>= funnel
                    let exp = Eram a b c d
                    id <- add_exp exp word_size
                    smash (word_size, ArgVar id)

(\/) :: (Bit a, Bit b) => a -> b -> Jazz Argument
x \/ y = binop Or x y

(/\) :: (Bit a, Bit b) => a -> b -> Jazz Argument
x /\ y = binop And x y

(<>) :: (Bit a, Bit b) => a -> b -> Jazz Argument
x <> y = binop Xor x y

conc :: (Wire a, Wire b) => a -> b -> Jazz [Argument]
conc x y = do w1 <- prog x
              w2 <- prog y
              return $ w1 ++ w2

funnel :: [Argument] -> Jazz (Integer, Argument)
funnel (x:xs) = do let aux (n,a) b = do id <- add_exp (Econcat b a) (n+1)
                                        return (n+1, ArgVar id)
                   foldM aux (1,x) xs

smash :: (Integer, Argument) -> Jazz [Argument]
smash (n, x) = mapM (\i -> select i x) [0..n-1]

select :: Bit a => Integer -> a -> Jazz Argument
select i x = do a <- funk x
                let exp = Eselect i a
                let n = 1
                id <- add_exp exp n
                return (ArgVar id)

squeeze :: Wire a => a -> Jazz [Argument]
squeeze xs = prog xs >>= funnel >>= smash

build :: Jazz () -> Netlist
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

