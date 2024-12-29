module Axis where

import Graphics.Rendering.OpenGL
import Data.Bool

data Projection' = Projection'
    { xAxis :: Point
    , yAxis :: Point
    , zAxis :: Point 
    } deriving (Eq,Show)

x = (1,0,0)
_x = (-1,0,0)
y = (0,1,0)
_y = (0,-1,0)
z = (0,0,1)
_z = (0,0,-1)

vector0 = (0.5,0,0)   :: Point
vector1 = (0.5,0.5,0) :: Point

type Point = (Double, Double, Double)

getModule :: Point -> Double
getModule (x,y,z) = sqrt((x^2) + (y^2) + (z^2))

getDotProduct :: Point -> Point -> Double
getDotProduct (x,y,z) (i,j,k) = ((x*i) + (y*j) + (z*k))

getAngleBetween :: Point -> Point -> Double
getAngleBetween (x,y,z) (i,j,k) = 
    let mod1 = getModule (x,y,z)
        mod2 = getModule (i,j,k)
        prod = getDotProduct (x,y,z) (i,j,k)
     in acos (prod / (mod1 * mod2))
    
getAngleX :: Point -> Double
getAngleX (x,y,z)
    | x < 0  && y < 0 = (+) (pi) $ getAngleBetween (-1,0,0) (x,y,0)
    | x > 0  && y < 0 = (+) (pi) $ getAngleBetween (-1,0,0) (x,y,0)
    | x == 0 && y < 0 = (+) (pi) $ getAngleBetween (-1,0,0) (x,y,0)
    | otherwise      = getAngleBetween (1,0,0) (x,y,0)

getAngleY :: Point -> Double
getAngleY (x,y,z)
    | y < 0  && z < 0 = (+) (pi) $ getAngleBetween (0,-1,0) (0,y,z)
    | y > 0  && z < 0 = (+) (pi) $ getAngleBetween (0,-1,0) (0,y,z)
    | y == 0 && z < 0 = (+) (pi) $ getAngleBetween (0,-1,0) (0,y,z)
    | otherwise       = getAngleBetween (0,1,0) (0,y,z)

getAngleZ :: Point -> Double
getAngleZ (x,y,z)
    | z < 0  && x < 0 = (+) (pi) $ getAngleBetween (0,0,-1) (x,0,z)
    | z > 0  && x < 0 = (+) (pi) $ getAngleBetween (0,0,-1) (x,0,z)
    | z == 0 && x < 0 = (+) (pi) $ getAngleBetween (0,0,-1) (x,0,z)
    | otherwise       = getAngleBetween (0,0,1) (x,0,z)

-- test --
test :: (Point -> Double) -> [(Double,Double,Double)] -> [Double] -> [Bool]
--test f p v = zipWith (\a b -> ((f a) - b) < 1e-9) p v
test f p v = zipWith (\a b -> ((f a) == b)) p v

x0 = [(1,0,0), (1,1,0), (0,1,0), (-1,1,0), (-1,0,0), (-1,-1,0), (0,-1,0), (1,-1,0)] :: [(Double,Double,Double)]
y0 = [(0,1,0), (0,1,1), (0,0,1), (0,-1,1), (0,-1,0), (0,-1,-1), (0,0,-1), (0,1,-1)] :: [(Double,Double,Double)]
z0 = [(0,0,1), (1,0,1), (1,0,0), (1,0,-1), (0,0,-1), (-1,0,-1), (-1,0,0), (-1,0,1)] :: [(Double,Double,Double)]
r0 = [0, pi/4, pi/2, 3*pi/4, pi, 5*pi/4, 3*pi/2, 7*pi/4]

-- Observer Conversions --

type Rad = Double

rotateZ :: Point -> Rad -> Point
rotateZ (x,y,z) n = if x == 0 && y == 0 then (0,0,z) else
    let angle = getAngleX (x,y,z)
        range = getModule (x,y,0)
     -- in ((*) (abs x) $ cos (angle +n), (*) (abs y) $ sin (angle +n), z) 
     in ((*) range $ cos (angle +n), (*) range $ sin (angle +n), z)
    
rotateY :: Point -> Rad -> Point
rotateY (x,y,z) n = if x == 0 && z == 0 then (0,y,0) else
    let angle = getAngleZ (x,y,z)
        range = getModule (x,0,z)
     --in ((*) (abs x) $ sin (angle +n), y, (*) (abs z) $ cos (angle +n))
     in ((*) range $ sin (angle +n), y, (*) range $ cos (angle +n))
    
rotateX :: Point -> Rad -> Point
rotateX (x,y,z) n = if y == 0 && z == 0 then (x,0,0) else
    let angle = getAngleY (x,y,z)
        range = getModule (0,y,z)
     -- in (x, (*) (abs y) $ cos (angle +n), (*) (abs z) $ sin (angle +n))
     in (x, (*) range $ cos (angle +n), (*) range $ sin (angle +n))

--rotateY :: Point -> Rad -> Point
--
--rotateY (x,y,z) n  =

