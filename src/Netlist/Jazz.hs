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

type Jazz = State Netmap

add_eq :: Equation -> Netmap -> Netmap
add_eq (id, exp) netmap =
  let n = exp_size exp netmap in
  Netmap { netmap_eqs   = Map.insert id exp (netmap_eqs netmap)
         , netmap_ids   = Map.insert exp id (netmap_ids netmap)
         , netmap_in    = netmap_in netmap
         , netmap_out   = netmap_out netmap
         , netmap_sizes = Map.insert id n (netmap_sizes netmap)
         }

create_wire :: Expression -> Jazz Ident
create_wire exp = do
  s <- get
  case Map.lookup exp (netmap_ids s) of
    Just id -> return id
    Nothing -> let id = "_x" ++ show (Map.size (netmap_ids s)) in
               let s' = add_eq (id, exp) s in
               put s' >> return id

build_netmap :: Jazz () -> Netmap
build_netmap x = (\(a,b) -> b) (runState x netmap_empty)

build_netlist :: Jazz () -> Netlist
build_netlist = netlist_from_netmap . build_netmap

newtype Bit = Bit Argument
class Bt a where
  bit :: a -> Jazz Bit

newtype Wire = Wire (Integer, Argument)
class Wr a where
  bits :: a -> Jazz [Bit]
  wire :: a -> Jazz Wire

wire_size :: Wr a => a -> Jazz Integer
wire_size x = wire x >>= (\(Wire(n,_)) -> return n)

input :: Ident -> Integer -> Jazz Wire
input id n = do s <- get
                let s' = Netmap { netmap_eqs   = netmap_eqs s
                                , netmap_ids   = netmap_ids s
                                , netmap_in    = Set.insert id (netmap_in s)
                                , netmap_out   = netmap_out s
                                , netmap_sizes = Map.insert id n (netmap_sizes s)
                                }
                put s'
                return $ Wire (n, ArgVar id)

output :: Wr a => Ident -> a -> Jazz ()
output id x = do Wire (n, a) <- wire x
                 let exp = Earg a
                 s <- get
                 let s' = Netmap { netmap_eqs   = Map.insert id exp (netmap_eqs s)
                                 , netmap_ids   = Map.insert exp id (netmap_ids s)
                                 , netmap_in    = netmap_in s
                                 , netmap_out   = Set.insert id (netmap_out s)
                                 , netmap_sizes = Map.insert id n (netmap_sizes s)
                                 }
                 put s'

new_reg :: Ident -> Integer -> Jazz ()
new_reg id n = do s <- get
                  put $ Netmap { netmap_eqs   = netmap_eqs s
                               , netmap_ids   = netmap_ids s
                               , netmap_in    = netmap_in s
                               , netmap_out   = netmap_out s
                               , netmap_sizes = Map.insert id n (netmap_sizes s)
                               }

reg_out :: Ident -> Jazz Wire
reg_out id = do s <- get
                let exp = Ereg id
                let n = arg_size (ArgVar id) s
                id' <- create_wire exp
                return $ Wire (n, ArgVar id')

reg_in :: Wr a => Ident -> a -> Jazz ()
reg_in id x = do Wire (n, a) <- wire x
                 let exp = Earg a
                 s <- get
                 let s' = Netmap { netmap_eqs   = Map.insert id exp (netmap_eqs s)
                                 , netmap_ids   = Map.insert exp id (netmap_ids s)
                                 , netmap_in    = netmap_in s
                                 , netmap_out   = netmap_out s
                                 , netmap_sizes = netmap_sizes s
                                 }
                 put s'

neg :: Bt a => a -> Jazz Bit
neg x = do Bit a <- bit x
           let exp = Enot a
           id <- create_wire exp
           return $ Bit (ArgVar id)

binop :: (Bt a, Bt b) => BinOp -> a -> b -> Jazz Bit
binop op x y = do Bit a <- bit x
                  Bit b <- bit y
                  let exp = Ebinop op a b
                  id <- create_wire exp
                  return $ Bit (ArgVar id)

(\/) :: (Bt a, Bt b) => a -> b -> Jazz Bit
x \/ y = binop Or x y

(/\) :: (Bt a, Bt b) => a -> b -> Jazz Bit
x /\ y = binop And x y

(<>) :: (Bt a, Bt b) => a -> b -> Jazz Bit
x <> y = binop Xor x y

mux :: (Bt a, Wr b, Wr c) => a -> b -> c -> Jazz Wire
mux x y z = do Bit a <- bit x
               Wire (n, b) <- wire y
               Wire (_, c) <- wire z
               let exp = Emux a b c
               id <- create_wire exp
               return $ Wire (n, ArgVar id)

rom :: Wr a => a -> Jazz Wire
rom x = do Wire (_, a) <- wire x
           let exp = Erom a
           id <- create_wire exp
           return $ Wire (word_size, ArgVar id)

ram :: (Wr a, Bt b, Wr c, Wr d) => a -> b -> c -> d -> Jazz Wire
ram ra we wa dt = do Wire (_, a) <- wire ra
                     Bit b <- bit we
                     Wire (_, c) <- wire wa
                     Wire (_, d) <- wire dt
                     let exp = Eram a b c d
                     id <- create_wire exp
                     return $ Wire (word_size, ArgVar id)

conc :: (Wr a, Wr b) => a -> b -> Jazz Wire
conc x y = do Wire (n, a) <- wire x
              Wire (m, b) <- wire y
              let exp = Econcat b a
              id <- create_wire exp
              return $ Wire (n+m, ArgVar id)

slice :: Wr a => Integer -> Integer -> a -> Jazz Wire
slice i j x = do Wire (n, a) <- wire x
                 let exp = Eslice i j a
                 id <- create_wire exp
                 return $ Wire (j-i, ArgVar id)

select :: Wr a => Integer -> a -> Jazz Bit
select i x = do Wire (_, a) <- wire x
                let exp = Eselect i a
                id <- create_wire exp
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

wire_of_integer :: (Integer, Integer) -> Wire
wire_of_integer (n, i) =
  let aux 0 _ = []
      aux n 0 = List.genericReplicate n False
      aux n i = (mod i 2 == 1):(aux (n-1) (div i 2))
  in
  Wire (n, ArgCst $ aux n i)

wire_of_bool_list :: [Bool] -> Wire
wire_of_bool_list l = Wire (List.genericLength l, ArgCst l)

instance Wr (Integer, Integer) where
  bits x = bits (wire_of_integer x)
  wire x = return (wire_of_integer x)

instance Wr a => Wr (Jazz a) where
  bits x = x >>= bits
  wire x = x >>= wire
instance Bt a => Wr [a] where
  bits x = mapM bit x
  wire x = do l <- mapM bit x
              let aux (Wire (n,a)) (Bit b) =
                    do id <- create_wire (Econcat b a)
                       return $ Wire ((n+1), (ArgVar id))
              case l of
                []         -> return $ Wire (0, ArgCst [])
                (Bit x):xs -> foldM aux (Wire (1, x)) xs

squeeze :: Wr a => a -> Jazz [Bit]
squeeze = bits . wire

