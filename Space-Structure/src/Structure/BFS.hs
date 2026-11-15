module Structure.BFS where

import qualified Control.Monad.State as State
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Structure.Graph as Graph

data BFS a = Struct [Graph.Vertex a] (Graph.Graph a)

connected :: Ord a => Graph.Graph a -> [Graph.Graph a]
connected = foldl f [] . Map.toList
    where
    f l (k,v) = if any (Map.member k) l then l else (Structure.BFS.scan v) : l

scan :: Ord a => Graph.Vertex a -> Map.Map a (Graph.Vertex a)
scan v = m where (Struct _ m) = Structure.BFS.start $ Struct [v] Map.empty

start :: Ord a => BFS a -> BFS a
start (Struct l m) = maybe (Struct l m) (start . f) (List.uncons l)
    where
    f (v,vs) = if Map.member (Graph.label v) m then Struct vs m
        else Struct 
            (foldl (flip (:)) vs (Graph.children v)) 
            (Map.insert (Graph.label v) v m) 

