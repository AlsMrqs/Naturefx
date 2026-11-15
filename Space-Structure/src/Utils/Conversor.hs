module Utils.Conversor where

import qualified Control.Monad.State as State
import qualified System.Random as Random
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Structure.Graph as Graph
import qualified Space as Space

data Manager a = Manager
    { dataset :: [(a,[a])]
    , visited :: Map.Map a (Graph.Vertex a) } 

remove :: Eq a => (a,[a]) -> Manager a -> Manager a
remove k (Manager l m) = Manager (List.delete k l) m

-- [!] map
insert :: Ord a => (a,Graph.Vertex a) -> Manager a -> Manager a
insert k (Manager l m) = Manager l (Map.insert (fst k) (snd k) m)

load :: Ord a => [(a,[a])] -> Manager a
load = flip Manager Map.empty 
    . map (fmap Set.toList) -- map snd
    . Map.toList            -- map fst
    . reflectEdges 
    . removeLoop

build :: Ord a => Manager a -> Graph.Graph a
build (Manager l m) = case List.uncons l of
    Nothing    -> m
    Just (s,_) -> build . snd . toGraph s $ Manager l m

toGraph :: Ord a => (a,[a]) -> Manager a -> (Graph.Vertex a, Manager a)
toGraph (v,vs) (Manager l m) = (,) newVtx newMngr
    where
    newVtx          = Graph.Vertex v edge
    thnkMngr        = Manager (List.delete (v,vs) l) (Map.insert v newVtx m)
    (newMngr, edge) = foldl visit (thnkMngr, []) vs

-- [!] understand

visit :: Ord a => (Manager a, [Graph.Vertex a]) -> a -> (Manager a, [Graph.Vertex a])
visit (Manager l m, edg) v = case Map.lookup v m of
    Just vtx -> (Manager l m, edg++[vtx])
    Nothing  -> (newMngr,     edg++[newVtx])
        where
        (newVtx, newMngr) = toGraph 
            (maybe (v,[]) id (List.find ((==) v. fst) l))
            (Manager l m)

empty :: Manager a
empty = Manager [] Map.empty

reflectEdges :: Ord a => [(a,[a])] -> Map.Map a (Set.Set a)
reflectEdges = List.foldl' (\acc x -> includeInversely (include acc x) x) Map.empty
    where
    include m (k,ks) = let united = Set.fromList ks in 
        Map.alter (return . maybe united (Set.union united)) k m

    includeInversely m (k,ks) = List.foldl' include m 
        $ map (flip (,) [k]) ks

removeLoop :: Ord a => [(a,[a])] -> [(a,[a])]
removeLoop = map (\(k,ks) -> (k,List.delete k ks))

