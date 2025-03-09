module Universe where

import qualified DBMS as DBMS
import Line
import Axis

import Data.List    (nub)

type Point  = (Double, Double, Double)
type Energy = (Double, Double, Double)

data Properties = Properties 
    { energy :: Energy 
    , others :: [Char] } deriving Show

instance Eq Properties where 
    (==) a b = (energy a, others a) == (energy b, others b)

data Object = Object 
    { point      :: Point 
    , properties :: Properties } deriving Show

instance Eq Object where 
    (==) a b = (point a, properties a) == (point b, properties b)

instance Ord Object where
    (<)  a b = point a < point b
    (>)  a b = point a > point b
    (<=) a b = a < b || a == b
    (>=) a b = a > b || a == b

data Coord = Coord 
    { number :: Double 
    , plane  :: [Object] } deriving Show

instance Eq Coord where
    (==) a b = (number a == number b)

instance Ord Coord where
    (<)  a b = (number a < number b)
    (>)  a b = (number a > number b)
    (<=) a b = a < b || a == b
    (>=) a b = a > b || a == b

instance Semigroup Coord where
    (<>) (Coord c0 p0) (Coord c1 p1) = Coord c0 (nub $ p0 ++ p1)

    -- Examples --
x0 = X (Node (Coord 0 []) Void Void)
c0 = Coord 0.0 [Object (0.0,0.0,0.0) (Properties (0.2,0.2,0.2) "")] 
c1 = Coord 0.1 [Object (0.1,0.1,0.1) (Properties (0.1,0.1,0.1) "")]

data Space a = Space (Axis a, Axis a, Axis a)

    -- todo --
-- we have (3) axis
-- this defines an space
-- we need a DBMS of objects
-- spaces will interact with it

-- data Space a = Space (Axis a, Axis a, Axis a) Database


