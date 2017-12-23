{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Netlist.Jazz where

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

newtype Bit = Bit Argument
class Bt a where
  bit :: a -> Jazz Bit

newtype Wire = Wire (Integer, Argument)
class Wr a where
  bits :: a -> Jazz [Bit]
  wire :: a -> Jazz Wire

arg_size :: Argument -> Jazz Integer
arg_size (ArgCst v) = return (List.genericLength v)
arg_size (ArgVar id) = do s <- get
                          let n = (env_sizes s) ! id
                          return n

input :: Ident -> Integer -> Jazz Wire
input id n = do s <- get
                let s' = Env { env_ids   = env_ids s
                             , env_in    = Set.insert id (env_in s)
                             , env_out   = env_out s
                             , env_sizes = Map.insert id n (env_sizes s)
                             }
                put s'
                return $ Wire (n, ArgVar id)

output :: Wr a => Ident -> a -> Jazz ()
output id x = do Wire (n, a) <- wire x
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

reg_out :: Ident -> Jazz Wire
reg_out id = do s <- get
                let exp = Ereg id
                let n = (env_sizes s) ! id
                id' <- add_exp exp n
                return $ Wire (n, ArgVar id')

reg_in :: Wr a => Ident -> a -> Jazz ()
reg_in id x = do Wire (n, ArgVar id') <- wire x
                 let exp = Earg (ArgVar id')
                 s <- get
                 let s' = Env { env_ids   = Map.insert exp id (env_ids s)
                              , env_in    = env_in s
                              , env_out   = env_out s
                              , env_sizes = env_sizes s
                              }
                 put s'

neg :: Bt a => a -> Jazz Bit
neg x = do Bit a <- bit x
           let exp = Enot a
           n <- arg_size a
           id <- add_exp exp n
           return $ Bit (ArgVar id)

binop :: (Bt a, Bt b) => BinOp -> a -> b -> Jazz Bit
binop op x y = do Bit a <- bit x
                  Bit b <- bit y
                  let exp = Ebinop op a b
                  n <- arg_size a
                  id <- add_exp exp n
                  return $ Bit (ArgVar id)

(\/) :: (Bt a, Bt b) => a -> b -> Jazz Bit
x \/ y = binop Or x y

(/\) :: (Bt a, Bt b) => a -> b -> Jazz Bit
x /\ y = binop And x y

(<>) :: (Bt a, Bt b) => a -> b -> Jazz Bit
x <> y = binop Xor x y

mux :: (Bt a, Wr b, Wr c) => a -> b -> c -> Jazz Wire
mux a xs ys = do Bit a <- bit a
                 Wire (_, ArgVar id') <- wire xs
                 Wire (_, y) <- wire ys
                 s <- get
                 let n = (env_sizes s) ! id'
                 let exp = Emux a (ArgVar id') y
                 id <- add_exp exp n
                 return $ Wire (n, ArgVar id)

rom :: Wr a => a -> Jazz Wire
rom x = do Wire (_, a) <- wire x
           let exp = Erom a
           id <- add_exp exp word_size
           return $ Wire (word_size, ArgVar id)

ram :: (Wr a, Bt b, Wr c, Wr d) => a -> b -> c -> d -> Jazz Wire
ram ra we wa dt = do Wire (_, a) <- wire ra
                     Bit b <- bit we
                     Wire (_, c) <- wire wa
                     Wire (_, d) <- wire dt
                     let exp = Eram a b c d
                     id <- add_exp exp word_size
                     return $ Wire (word_size, ArgVar id)

conc :: (Wr a, Wr b) => a -> b -> Jazz Wire
conc x y = do Wire (n, a) <- wire x
              Wire (m, b) <- wire y
              let exp = Econcat b a
              id <- add_exp exp (n+m)
              return $ Wire (n+m, ArgVar id)

slice :: Wr a => Integer -> Integer -> a -> Jazz Wire
slice i j x = do Wire (n, a) <- wire x
                 let exp = Eslice i j a
                 id <- add_exp exp (j-i)
                 return $ Wire (j-i, ArgVar id)

select :: Wr a => Integer -> a -> Jazz Bit
select i x = do Wire (_, a) <- wire x
                let exp = Eselect i a
                let n = 1
                id <- add_exp exp n
                return $ Bit (ArgVar id)

instance Bt Bit where
  bit x = return x
instance Bt a => Bt (Jazz a) where
  bit x = x >>= bit
instance Bt Bool where
  bit x = return $ Bit (ArgCst [x])
instance Bt Integer where
  bit 0 = return $ Bit (ArgCst [False])
  bit 1 = return $ Bit (ArgCst [True])

instance Wr Wire where
  bits (Wire (n, x)) = mapM (\i -> select i (Wire (n, x))) [0..n-1]
  wire x = return x
instance Wr a => Wr (Jazz a) where
  bits x = x >>= bits
  wire x = x >>= wire
instance Bt a => Wr [a] where
  bits x = mapM bit x
  wire x = do l <- mapM bit x
              let aux (Wire (n,a)) (Bit b) =
                    do id <- add_exp (Econcat b a) (n+1)
                       return $ Wire ((n+1), (ArgVar id))
              case l of
                []         -> return $ Wire (0, ArgCst [])
                (Bit x):xs -> foldM aux (Wire (1, x)) xs

squeeze :: Wr a => a -> Jazz [Bit]
squeeze = bits . wire

