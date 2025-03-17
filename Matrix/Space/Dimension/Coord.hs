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
takeCoordinate (Node _ x _) = Just $ coordinate x

haveObject :: Coord -> Object -> Bool
haveObject = flip elem . plane 

insertObject :: Coord -> Object -> Coord
insertObject k obj = 
    if elem obj $ plane k
        then k
        else Coord (coordinate k) ((:) obj $ plane k)

removeObject :: Coord -> Object -> Coord
removeObject k obj =
    if elem obj $ plane k
        then Coord (coordinate k) ((\\) [obj] $ plane k)
        else k

    -- definir a instância necessária --

type ModifyedLine a = Line a

insertCoord :: Line Coord -> Coord -> Line Coord
insertCoord Void          k = Node Void k Void 
insertCoord (Node l x r) k 
    | coordinate x == coordinate k = 
        if x == k 
            then Node l x r
            else 
                let link = Node l1 (merge x k) r1
                    l1   = updateL l link
                    r1   = updateR r link
                 in link
    | coordinate x <  coordinate k = 
        let newLink = Node l1 x r1
            l1      = updateL l newLink
            r1      = insertR r (newLink, k)
         in newLink
    | coordinate x >  coordinate k = 
        let newLink = Node l1 x r1
            r1      = updateR r newLink
            l1      = insertL l (newLink, k)
         in newLink

merge :: Coord -> Coord -> Coord
merge k = Coord (coordinate k) . nub . (++) (plane k) . plane

updateR (Void)        (link) = Void
updateR (Node l x r) (link) = newLink
    where
        newLink = Node link x r1
        r1       = updateR r newLink

updateL (Void)        (link) = Void
updateL (Node l x r) (link) = newLink
    where
        newLink = Node l1 x link
        l1       = updateL l newLink
        
insertR :: Line Coord -> (Line Coord, Coord) -> Line Coord
insertR (Void)        (link, k) = Node link k Void 
insertR (Node l x r) (link, k) 
    | coordinate x == coordinate k = 
        let newLink = Node link (merge x k) r1
            r1       = updateR r newLink
         in newLink
    | coordinate x <  coordinate k =
        let newLink = Node link x r1
            r1       = insertR r (newLink, k)
         in newLink
    | coordinate x >  coordinate k =
        let newLink = Node link k r1
            r1       = insertR r (newLink, x)
         in newLink

insertL :: Line Coord -> (Line Coord, Coord) -> Line Coord
insertL (Void)        (link, k) = Node Void k link 
insertL (Node l x r) (link, k)
    | coordinate x == coordinate k =
        let newLink = Node l1 (merge x k) link
            l1       = updateL l newLink
         in newLink
    | coordinate x <  coordinate k =
        let newLink = Node l1 k link
            l1       = insertL l (newLink, x)
         in newLink
    | coordinate x >  coordinate k =
        let newLink = Node l1 x link
            l1       = insertL l (newLink, k)
         in newLink

    -- data Object --

data Object = Object (Double, Double, Double) (Double, Double, Double) 

instance Show Object where
    show (Object (a,b,c) (x,y,z)) = "\n\tOBJ " ++ show (xNode a, yNode b, zNode c) ++ show (x,y,z)

instance Eq Object where
    (==) (Object (a0,b0,c0) (a,b,c)) (Object (x0,y0,z0) (x,y,z)) = (a,b,c) == (x,y,z) 
        && (xNode a0, yNode b0, zNode c0) == (xNode x0, yNode y0, zNode z0) 

xNode = takeCoordinate . line
yNode = takeCoordinate . line
zNode = takeCoordinate . line

updateObject :: Object -> (Axis Coord, Axis Coord, Axis Coord) -> Object
updateObject (Object _ (x, y, z)) (xn, yn, zn) = Object (xn, yn, zn) (x, y, z)

    -- Create a (Move) function that update Vecto Space --
    
--neo :: (Axis Coord, Axis Coord, Axis Coord) -> Object -> ((Axis Coord, Axis Coord, Axis Coord), Object)
--neo (x0,y0,z0) obj@(Object (x,y,z) (kx, ky, kz)) =
--    let (x1, xn)    = moveX (x0, (yn, zn)) (newObject, newCoord (kx,x), obj) 
--        (y1, yn)    = moveY (y0, (xn, zn)) (newObject, newCoord (ky,y), obj) 
--        (z1, zn)    = moveZ (z0, (xn, yn)) (newObject, newCoord (kz,z), obj) 
--        newObject   = updateObject obj (xn, yn, zn)
--        newCoord    = \(a,b) -> (+) a . fromMaybe 0 $ takeCoordinate . line $ b
--    in  ((x1,y1,z1), newObject)

type NewAxis a = Axis a
type NewObject = Object

moveX :: (Axis Coord, (NewAxis Coord, NewAxis Coord)) -> (Object, Double, Object) -> NewAxis Coord
moveX (x0, (yn, zn)) (newObj, xIndex, obj) =
    let lineX  = line x0 
        xn     = reconstructor x0 
            . (flip insertCoord $ Coord xIndex [newObj]) 
            . fmap_ (flip removeObject $ obj) $ lineX
     in xn

moveY :: (Axis Coord, (NewAxis Coord, NewAxis Coord)) -> (Object, Double, Object) -> NewAxis Coord
moveY (y0, (xn, zn)) (newObj, yIndex, obj) =
    let lineY = line y0 
        yn    = reconstructor y0
            . (flip insertCoord $ Coord yIndex [newObj])
            . fmap_ (flip removeObject $ obj) $ lineY
     in yn

moveZ :: (Axis Coord, (NewAxis Coord, NewAxis Coord)) -> (Object, Double, Object) -> NewAxis Coord
moveZ (z0, (xn, yn)) (newObj, zIndex, obj) =
    let lineZ = line z0 
        zn    = reconstructor z0
            . (flip insertCoord $ Coord zIndex [newObj]) 
            . fmap_ (flip removeObject $ obj) $ lineZ
     in zn

    -- test examples --
obj_0 = Object (x0,y0,z0) (-0.1,0.01,0.2)
x0 = X $ Node Void (Coord 0 []) Void
y0 = Y $ Node Void (Coord 0 []) Void
z0 = Z $ Node Void (Coord 0 []) Void

