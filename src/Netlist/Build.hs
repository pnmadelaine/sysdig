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
  bit :: a -> Jazz Argument

class Wire a where
  bits :: a -> Jazz [Argument]
  wire :: a -> Jazz Argument


instance Bit a => Wire [a] where
  bits l = mapM bit l
  wire l = mapM bit l >>= funnel >>= (\(_, a) -> return a)

instance Wire a => Wire (Jazz a) where
  bits x = x >>= bits
  wire x = x >>= wire

instance Bit Argument where
  bit x = return x

instance Bit a => Bit (Jazz a) where
  bit x = x >>= bit

instance Bit Bool where
  bit x = return $ ArgCst [x]

instance Bit Integer where
  bit 0 = return $ ArgCst [False]
  bit 1 = return $ ArgCst [True]

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
output id l = do w <- bits l
                 (n, a) <- funnel w
                 let exp = Earg a
                 s <- get
                 let s' = Env { env_ids   = Map.insert exp id $ env_ids s
                              , env_in    = env_in s
                              , env_out   = Set.insert id $ env_out s
                              , env_sizes = Map.insert id n $ env_sizes s
                              }
                 put s'

new_reg :: Ident -> Integer -> Jazz ()
new_reg id n = do s <- get
                  let s' = Env { env_ids   = env_ids s
                               , env_in    = env_in s
                               , env_out   = Set.insert id $ env_out s
                               , env_sizes = Map.insert id n $ env_sizes s
                               }
                  put s'

reg_out :: Ident -> Jazz [Argument]
reg_out id = do s <- get
                let exp = Ereg id
                let n = (env_sizes s) ! id
                id' <- add_exp exp n
                smash (n, ArgVar id')

reg_in :: Wire a => Ident -> a -> Jazz ()
reg_in id x = do (n, ArgVar id') <- bits x >>= funnel
                 let exp = Earg (ArgVar id')
                 s <- get
                 let s' = Env { env_ids   = Map.insert exp id (env_ids s)
                              , env_in    = env_in s
                              , env_out   = env_out s
                              , env_sizes = env_sizes s
                              }
                 put s'

neg :: Bit a => a -> Jazz Argument
neg x = do a <- bit x
           let exp = Enot a
           n <- arg_size a
           id <- add_exp exp n
           return (ArgVar id)

binop :: (Bit a, Bit b) => BinOp -> a -> b -> Jazz Argument
binop op x y = do a <- bit x
                  b <- bit y
                  let exp = Ebinop op a b
                  n <- arg_size a
                  id <- add_exp exp n
                  return (ArgVar id)

mux :: (Bit a, Wire b, Wire c) => a -> b -> c -> Jazz [Argument]
mux a xs ys = do a <- bit a
                 (ArgVar id') <- wire xs
                 y <- wire ys
                 s <- get
                 let n = (env_sizes s) ! id'
                 let exp = Emux a (ArgVar id') y
                 id <- add_exp exp n
                 smash (n, (ArgVar id))

rom :: Wire a => a -> Jazz [Argument]
rom xs = do (_, a) <- bits xs >>= funnel
            let exp = Erom a
            id <- add_exp exp word_size
            smash (word_size, ArgVar id)

ram :: (Wire a, Bit b, Wire c, Wire d) => a -> b -> c -> d -> Jazz [Argument]
ram ra we wa d = do (_, a) <- bits ra >>= funnel
                    b <- bit we
                    (_, c) <- bits wa >>= funnel
                    (_, d) <- bits d  >>= funnel
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
conc x y = do w1 <- bits x
              w2 <- bits y
              return $ w1 ++ w2

funnel :: [Argument] -> Jazz (Integer, Argument)
funnel (x:xs) = do let aux (n,a) b = do id <- add_exp (Econcat b a) (n+1)
                                        return (n+1, ArgVar id)
                   foldM aux (1,x) xs

smash :: (Integer, Argument) -> Jazz [Argument]
smash (n, x) = mapM (\i -> select i x) [0..n-1]

select :: Bit a => Integer -> a -> Jazz Argument
select i x = do a <- bit x
                let exp = Eselect i a
                let n = 1
                id <- add_exp exp n
                return (ArgVar id)

squeeze :: Wire a => a -> Jazz [Argument]
squeeze xs = bits xs >>= funnel >>= smash

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

