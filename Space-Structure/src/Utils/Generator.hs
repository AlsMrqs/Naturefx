module Utils.Generator where

import qualified Control.Monad.State as State
import qualified System.Random as Random
import qualified Data.List as List
import qualified Data.Bool as Bool
import qualified Data.Set as Set
import qualified Space as Space -- [!] unecessary

generate :: Int -> IO [(Space.Point,[Space.Point])]
generate k = vectors k Set.empty >>= \vtx -> sequence 
    $ build (Set.size vtx) (Set.toList vtx)

vectors :: Int -> Set.Set Space.Point -> IO (Set.Set Space.Point)
vectors k s
    | Set.size s == k || k < 0 = return s
    | otherwise                = Space.randomPosition 
        >>= return . flip Set.insert s 
        >>= vectors k

build :: Int -> [a] -> [IO (a,[a])]
build n lst = map link lst
    where
    link k = do
        len <- randomRange n
        idx <- fmap Set.toList $ genIdx len n Set.empty
        return (k,(filterIdx idx) lst)

filterIdx :: [Int] -> [a] -> [a]
filterIdx idx lst = fst 
    $ List.foldl' (flip match) ([],idx) (zip lst [1..])

type Base a = (a,Int)
type Roof a = ([a],[Int])

match :: Base a -> Roof a -> Roof a
match _ (k,[]) = (k,[])
match s (k,ks) = Bool.bool (k,ks) (fst s:k,is) (snd s == i)
    where
    (i:is) = ks

type Batch = Int
type Range = Int

genIdx :: Batch -> Range -> Set.Set Int -> IO (Set.Set Int)
genIdx k n s = if any ((==) (Set.size s)) [k,n]
    then return s
    else randomRange n >>= genIdx k n . flip Set.insert s

randomRange :: Int -> IO Int
randomRange k = return . fst . Random.randomR (0,k) =<< Random.newStdGen

