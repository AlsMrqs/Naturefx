module Plane where

import Data.Bool    (bool)

import Data.List    (intercalate, intersperse)
import Prelude  hiding  (Right, Left)

data Plane = Plane
    { limit :: (Int,Int)
    , grid  :: [[Elem]]
    , solid :: [(Int,Int)] } 

instance Show Plane where
    show = intercalate "\n" . map (intersperse ' ' . map pixel) . grid 

data Elem = Elem
    { point :: (Int,Int)
    , pixel :: Char } deriving Show

data Direction = Up | Down | Left | Right deriving Show

type Point = (Int,Int)
--type Block = [Elem] -- to change --
type Pixel = Char

newPlane :: Point -> Plane
newPlane (n,m) = Plane (x,y) freeZone deadZone
    where
        (x,y) = (n,m)

        xs = [-x..x]
        ys = [y,(y-1)..(-y)]

        freeZone = [[Elem (a,b) '+' | a <- xs] | b <- ys] 
        deadZone = 
               (zip (repeat $ -x-1) ys) ++
               (zip (repeat $  x+1) ys) ++
               (zip xs (repeat $ -y-1))

with :: Plane -> [Elem] -> Plane
with _plane _block = Plane (limit _plane) newFreeZone (solid _plane)
    where
        newFreeZone     = map mutate (grid _plane)
        mutate          = map (\_element -> foldl swapPixel _element _block)
        swapPixel acc x = bool acc (shiftPixel acc x) $ samePoint acc x

samePoint :: Elem -> Elem -> Bool
samePoint _element = (==) (point _element) . point 

shiftPixel :: Elem -> Elem -> Elem
shiftPixel _element = changePixel _element . pixel

changePixel :: Elem -> Pixel -> Elem
changePixel _element = Elem (point _element)

addSolid :: Plane -> [Point] -> Plane
addSolid _plane = Plane (limit _plane) (grid _plane) . (++) (solid _plane)

falls :: Plane -> [Elem] -> Bool
falls _plane = any $ (\(a,b) -> ((abs a) > x) || ((abs b) > y)) . point
    where
        (x,y) = limit _plane

