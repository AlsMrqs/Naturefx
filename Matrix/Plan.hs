module Matrix.Plan where 

import Data.List
import Data.Bool (bool)

getY = snd . fst 
getX = fst . fst

type E = (P,Char)
type P = (Int,Int)

plan :: Int -> Int -> [[E]]
plan x y = [[(,) (a,b) ' ' | a <- [-x..x]] | b <- [y,(y-1)..(-y)]]

func :: (Int -> Int) -> [[E]] -> [[E]]
func g = map (map h)
    where
    h ((,) (x,y) c) = bool ((,) (x,y) c) ((,) (x,y) '+') $ y == g x

show :: [[E]] -> IO ()
show = putStrLn . intercalate "\n" . map (intersperse ' ' . map snd)

