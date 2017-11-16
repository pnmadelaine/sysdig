module Graph where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad (guard, foldM)

type Graph a = Map.Map a [a]

empty = Map.empty

add_node :: Ord a => a -> Graph a -> Graph a
add_node x graph = Map.insertWith (\_ l -> l) x [] graph

-- TODO: check if x \in G and y \in G
add_edge :: Ord a => a -> a -> Graph a -> Graph a
add_edge x y graph = Map.insertWith (\hd tl -> hd++tl) x [y] (add_node y graph)

neigh :: Ord a => a -> Graph a -> [a]
neigh x graph = case Map.lookup x graph of
                  Just l  -> l

list_nodes :: Ord a => Graph a -> [a]
list_nodes = Map.keys

topological :: Ord a => Graph a -> Either String [a]
topological graph =
  let m = (Set.empty, []) in
  case foldM (dfs Set.empty) m (list_nodes graph) of
    Nothing      -> Left "combinational cycle"
    Just (_,ord) -> Right ord
    where dfs currentPath (checked, ord) x = do
            guard $ not (Set.member x currentPath)
            if Set.member x checked then
              return (checked, ord)
            else do
              let newPath = Set.insert x currentPath
              (checked', ord') <- foldM (dfs newPath) (checked, ord) (neigh x graph)
              return (Set.insert x checked', x:ord') 

