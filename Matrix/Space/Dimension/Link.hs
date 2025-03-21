module Coord where

import Line
import Axis

import Data.List    ((\\), nub)
import Data.Maybe   (fromMaybe, isNothing)
import Data.Bool    (bool)

data Coord = Coord
    { coordinate :: Double
    , plane      :: [Object]
    } --deriving Show

instance Show Coord where
    show (Coord dbl pln) = "\n(" ++ show dbl ++ ")" ++ show pln

instance Eq Coord where
    (==) (Coord x n) (Coord a m) = (x,n) == (a,m)

takeCoordinate :: Line Coord -> Maybe Double
takeCoordinate Void          = Nothing
takeCoordinate (Point _ x _) = Just $ coordinate x

haveObject :: Coord -> Object -> Bool
haveObject = flip elem . plane 

insertObject :: Coord -> Object -> Coord
insertObject lnk obj = 
    if elem obj $ plane lnk
        then lnk
        else Coord (coordinate lnk) ((:) obj $ plane lnk)

removeObject :: Coord -> Object -> Coord
removeObject lnk obj =
    if elem obj $ plane lnk
        then Coord (coordinate lnk) ((\\) [obj] $ plane lnk)
        else lnk

    -- definir a instância necessária --

type ModifyedLine a = Line a

insertCoord :: Line Coord -> Coord -> (Line Coord, ModifyedLine Coord)
insertCoord Void          lnk = let ln = Point Void lnk Void in (ln, ln)
insertCoord (Point l x r) lnk 
    | coordinate x == coordinate lnk = 
        if x == lnk 
            then
                let ln = Point l x r
                 in (ln, ln)
            else 
                let ln = Point l1 (merge x lnk) r1
                    l1 = updateL l ln
                    r1 = updateR r ln
                 in (ln, ln)
    | coordinate x <  coordinate lnk = 
        let currPrev  = Point l1 x r1
            l1        = updateL l currPrev
            (r1, ptr) = insertR r (currPrev, lnk)
         in (currPrev, ptr)
    | coordinate x >  coordinate lnk = 
        let currPrev  = Point l1 x r1
            r1        = updateR r currPrev
            (l1, ptr) = insertL l (currPrev, lnk)
         in (currPrev, ptr)

merge :: Coord -> Coord -> Coord
merge lnk = Coord (coordinate lnk) . nub . (++) (plane lnk) . plane

updateR (Void)        (newPrev) = Void
updateR (Point l x r) (newPrev) = currPrev
    where
        currPrev = Point newPrev x r1
        r1       = updateR r currPrev

updateL (Void)        (newPrev) = Void
updateL (Point l x r) (newPrev) = currPrev
    where
        currPrev = Point l1 x newPrev
        l1       = updateL l currPrev
        
insertR :: Line Coord -> (Line Coord, Coord) -> (Line Coord, Line Coord)
insertR (Void)        (newPrev, lnk) = let ln = Point newPrev lnk Void in (ln, ln)
insertR (Point l x r) (newPrev, lnk) 
    | coordinate x == coordinate lnk = 
        let currPrev = Point newPrev (merge x lnk) r1
            r1       = updateR r currPrev
         in (currPrev, currPrev)
    | coordinate x <  coordinate lnk =
        let currPrev  = Point newPrev x r1
            (r1, ptr) = insertR r (currPrev, lnk)
         in (currPrev, ptr)
    | coordinate x >  coordinate lnk =
        let currPrev  = Point newPrev lnk r1
            (r1, ptr) = insertR r (currPrev, x)
         in (currPrev, ptr)

insertL :: Line Coord -> (Line Coord, Coord) -> (Line Coord, Line Coord)
insertL (Void)        (newPrev, lnk) = let ln = Point Void lnk newPrev in (ln, ln)
insertL (Point l x r) (newPrev, lnk)
    | coordinate x == coordinate lnk =
        let currPrev = Point l1 (merge x lnk) newPrev
            l1       = updateL l currPrev
         in (currPrev, currPrev)
    | coordinate x <  coordinate lnk =
        let currPrev  = Point l1 lnk newPrev
            (l1, ptr) = insertL l (currPrev, x)
         in (currPrev, ptr)
    | coordinate x >  coordinate lnk =
        let currPrev  = Point l1 x newPrev
            (l1, ptr) = insertL l (currPrev, lnk)
         in (currPrev, ptr)

    -- data Object --

data Object = Object (Axis Coord, Axis Coord, Axis Coord) (Double, Double, Double) 

instance Show Object where
    show (Object (a,b,c) (x,y,z)) = "\n\tOBJ " ++ show (xPoint a, yPoint b, zPoint c) ++ show (x,y,z)

instance Eq Object where
    (==) (Object (a0,b0,c0) (a,b,c)) (Object (x0,y0,z0) (x,y,z)) = (a,b,c) == (x,y,z) 
        && (xPoint a0, yPoint b0, zPoint c0) == (xPoint x0, yPoint y0, zPoint z0) 

xPoint = takeCoordinate . line
yPoint = takeCoordinate . line
zPoint = takeCoordinate . line

updateObject :: Object -> (Axis Coord, Axis Coord, Axis Coord) -> Object
updateObject (Object _ (x, y, z)) (xn, yn, zn) = Object (xn, yn, zn) (x, y, z)

    -- Create a (Move) function that update Vecto Space --
    
neo :: (Axis Coord, Axis Coord, Axis Coord) -> Object -> ((Axis Coord, Axis Coord, Axis Coord), Object)
neo (x0,y0,z0) obj@(Object (x,y,z) (kx, ky, kz)) =
    let (x1, xn)    = moveX (x0, (yn, zn)) (newObject, newCoord (kx,x), obj) 
        (y1, yn)    = moveY (y0, (xn, zn)) (newObject, newCoord (ky,y), obj) 
        (z1, zn)    = moveZ (z0, (xn, yn)) (newObject, newCoord (kz,z), obj) 
        newObject   = updateObject obj (xn, yn, zn)
        newCoord    = \(a,b) -> (+) a . fromMaybe 0 $ takeCoordinate . line $ b
    in  ((x1,y1,z1), newObject)

type NewAxis a = Axis a
type NewObject = Object

moveX :: (Axis Coord, (NewAxis Coord, NewAxis Coord)) -> (Object, Double, Object) -> (NewAxis Coord, NewAxis Coord)
moveX (x0, (yn, zn)) (newObj, xIndex, obj) =
    let lineX       = line x0 
        (xn, ptr)   = (\(a,b) -> (reconstructor x0 a, reconstructor x0 b))
            . (flip insertCoord $ Coord xIndex [newObj]) 
            . fmap_ (flip removeObject $ obj) $ lineX
     in (xn, ptr)

moveY :: (Axis Coord, (NewAxis Coord, NewAxis Coord)) -> (Object, Double, Object) -> (NewAxis Coord, NewAxis Coord)
moveY (y0, (xn, zn)) (newObj, yIndex, obj) =
    let lineY       = line y0 
        (yn, ptr)   = (\(a,b) -> (reconstructor y0 a, reconstructor y0 b))
            . (flip insertCoord $ Coord yIndex [newObj])
            . fmap_ (flip removeObject $ obj) $ lineY
     in (yn, ptr)

moveZ :: (Axis Coord, (NewAxis Coord, NewAxis Coord)) -> (Object, Double, Object) -> (NewAxis Coord, NewAxis Coord)
moveZ (z0, (xn, yn)) (newObj, zIndex, obj) =
    let lineZ       = line z0 
        (zn, ptr)   = (\(a,b) -> (reconstructor z0 a, reconstructor z0 b))
            . (flip insertCoord $ Coord zIndex [newObj]) 
            . fmap_ (flip removeObject $ obj) $ lineZ
     in (zn, ptr)

    -- test examples --
obj_0 = Object (x0,y0,z0) (-0.1,0.01,0.2)
x0 = X $ Point Void (Coord 0 []) Void
y0 = Y $ Point Void (Coord 0 []) Void
z0 = Z $ Point Void (Coord 0 []) Void

