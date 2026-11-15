module Space where

import qualified System.Random as Random

data Axis = X | Y | Z

class Coord a where 
    coord :: Axis -> a -> Double

newtype Point = Point (Double, Double, Double)

instance Eq Point where
    (==) (Point a) (Point b) = a == b

instance Ord Point where
    (<)  (Point a) (Point b) = a < b
    (<=) (Point a) (Point b) = a <= b
    (>)  (Point a) (Point b) = a > b
    (>=) (Point a) (Point b) = a >= b

instance Coord Point where
    coord X (Point (x,_,_)) = x
    coord Y (Point (_,y,_)) = y
    coord Z (Point (_,_,z)) = z

point :: Coord a => a -> Space.Point
point = \k -> Point (coord X k, coord Y k, coord Z k)

-- [!] Generator

generateCoord:: IO Double
generateCoord = Random.newStdGen >>= return . fst . Random.randomR (-1,1)

randomPosition :: IO Point
randomPosition = do
    x <- return . roundTo 2 =<< Space.generateCoord
    y <- return . roundTo 2 =<< Space.generateCoord
    z <- return . roundTo 2 =<< Space.generateCoord
    return $ Point (x,y,z)

roundTo :: Int -> Double -> Double
roundTo n = flip (/) (10^n) . fromIntegral . (round :: Double -> Int) . (*) (10^n) 

