module Link where

import Line
import Axis

import Data.List  ((\\), nub)
import Data.Maybe (fromMaybe, isNothing)

data Link = Link
    { coordinate :: Double
    , plane      :: [Object]
    } deriving Show

instance Eq Link where
    (==) (Link x n) (Link a m) = (x,n) == (a,m)

    -- take (Line a) {id} content --
takeCoordinate :: Line Link -> Maybe Double
takeCoordinate Void          = Nothing
takeCoordinate (Point _ x _) = Just $ coordinate x
    --

haveObject :: Link -> Object -> Bool
haveObject (Link _ list) obj = elem obj list

insertObject :: Link -> Object -> Link
insertObject lnk@(Link n lst) obj = 
    if elem obj lst 
        then lnk 
        else (Link n (obj:lst))

removeObject :: Link -> Object -> Link
removeObject lnk@(Link n lst) obj = 
    if elem obj lst 
        then (Link n (lst \\ [obj]))
        else lnk

merge :: Link -> Link -> Link
merge (Link n pln) lnk = Link n (nub . (++) pln $ plane lnk)

    -- Insert Link --

type ModifyedLine a = Line a

insertLink :: Line Link -> Link -> (Line Link, ModifyedLine Link)
insertLink Void          lnk = let ln = Point Void lnk Void in (ln, ln)
insertLink (Point l x r) lnk 
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
            (r1, ptr) = insertR r (currPrev, lnk) -- Here --
         in (currPrev, ptr)
    | coordinate x >  coordinate lnk = 
        let currPrev  = Point l1 x r1
            r1        = updateR r currPrev
            (l1, ptr) = insertL l (currPrev, lnk)
         in (currPrev, ptr)

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
        
        -- Here --
insertR :: Line Link -> (Line Link, Link) -> (Line Link, Line Link)
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

insertL :: Line Link -> (Line Link, Link) -> (Line Link, Line Link)
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

--insertLink :: Line Link -> Link -> Line Link
--insertLink Void          lnk = Point Void lnk Void
--insertLink (Point l x r) lnk 
--    | coordinate x == coordinate lnk = Point l (merge x lnk) r
--    | coordinate x <  coordinate lnk = 
--        let currPrev = Point l1 x r1
--            l1       = updateL l currPrev
--            r1       = insertR r (currPrev, lnk)
--         in currPrev
--    | coordinate x >  coordinate lnk = 
--        let currPrev = Point l1 x r1
--            r1       = updateR r currPrev
--            l1       = insertL l (currPrev, lnk)
--         in currPrev
--
--updateR (Void)        (newPrev) = Void
--updateR (Point l x r) (newPrev) = currPrev
--    where
--        currPrev = Point newPrev x r1
--        r1       = updateR r currPrev
--
--updateL (Void)        (newPrev) = Void
--updateL (Point l x r) (newPrev) = currPrev
--    where
--        currPrev = Point l1 x newPrev
--        l1       = updateL l currPrev
--        
--insertR (Void)        (newPrev, lnk) = Point newPrev lnk Void
--insertR (Point l x r) (newPrev, lnk) = currPrev
--    where
--        currPrev = Point newPrev x r1
--        r1       = insertR r (currPrev, lnk)
--        
--insertL (Void)        (newPrev, lnk) = Point Void lnk newPrev
--insertL (Point l x r) (newPrev, lnk) = currPrev
--    where
--        currPrev = Point l1 x newPrev
--        l1       = insertL l (currPrev, lnk)

--P Void a Void
--let curPrev = P Void a newNext
--    newNext = P currPrev lnk Void

data Object = Object (Axis Link, Axis Link, Axis Link) (Double, Double, Double) 

instance Show Object where
    show (Object (a,b,c) (x,y,z)) = "Object " ++ show (xPoint a, yPoint b, zPoint c) ++ show (x,y,z)

instance Eq Object where
    (==) (Object (a0,b0,c0) (a,b,c)) (Object (x0,y0,z0) (x,y,z)) = 
        (a,b,c) == (x,y,z)
        && (xPoint a0, yPoint b0, zPoint c0) == (xPoint x0, yPoint y0, zPoint z0) 

xPoint = takeCoordinate . line
yPoint = takeCoordinate . line
zPoint = takeCoordinate . line

updateObject :: Object -> (Axis Link, Axis Link, Axis Link) -> Object
updateObject (Object _ (x, y, z)) (xn, yn, zn) = Object (xn, yn, zn) (x, y, z)

obj_0 = Object (x0,y0,z0) (-1,0.1,2)
x0 = X $ Point Void (Link 0 []) Void
y0 = Y $ Point Void (Link 0 []) Void
z0 = Z $ Point Void (Link 0 []) Void

neo :: (Axis Link, Axis Link, Axis Link) -> Object -> ((Axis Link, Axis Link, Axis Link), Object)
neo (x0,y0,z0) obj@(Object (x,y,z) (kx, ky, kz)) =
    let (x1, xn)    = moveX (x0, (yn, zn)) (newObject, newCoord (kx,x), obj) --(+) kx . fromMaybe 0 $ takeCoordinate . line $ x)
        (y1, yn)    = moveY (y0, (xn, zn)) (newObject, newCoord (ky,y), obj) --(+) ky . fromMaybe 0 $ takeCoordinate . line $ y)
        (z1, zn)    = moveZ (z0, (xn, yn)) (newObject, newCoord (kz,z), obj) --(+) kz . fromMaybe 0 $ takeCoordinate . line $ z)
        newObject   = updateObject obj (xn, yn, zn)
        newCoord    = \(a,b) -> (+) a . fromMaybe 0 $ takeCoordinate . line $ b
    in  ((x1,y1,z1), newObject)

type NewAxis a = Axis a
type NewObject = Object

    -- Move Object --
moveX :: (Axis Link, (NewAxis Link, NewAxis Link)) -> (Object, Double, Object) -> (NewAxis Link, NewAxis Link)
moveX (x0, (yn, zn)) (newObj, xIndex, obj) =  --obj@(Object (x, y, z) (kx, ky, kz)) = 
    let --newObj      = updateObject obj (ptr, yn, zn)
        --xIndex      = (+) kx . fromMaybe 0 $ takeCoordinate . line $ x
        lineX       = line x0 
        (xn, ptr)   = (\(a,b) -> (reconstructor x0 a, reconstructor x0 b))
            . (flip insertLink $ Link xIndex [newObj]) 
            . fmap_ (flip removeObject $ obj) $ lineX
     in (xn, ptr)

moveY :: (Axis Link, (NewAxis Link, NewAxis Link)) -> (Object, Double, Object) -> (NewAxis Link, NewAxis Link)
moveY (y0, (xn, zn)) (newObj, yIndex, obj) = -- obj@(Object (x, y, z) (kx, ky, kz)) = 
    let --newObj      = updateObject obj (xn, ptr, zn)
        --yIndex      = (+) ky . fromMaybe 0 $ takeCoordinate . line $ y
        lineY       = line y0 
        (yn, ptr)   = (\(a,b) -> (reconstructor y0 a, reconstructor y0 b))
            . (flip insertLink $ Link yIndex [newObj])
            . fmap_ (flip removeObject $ obj) $ lineY
     in (yn, ptr)

moveZ :: (Axis Link, (NewAxis Link, NewAxis Link)) -> (Object, Double, Object) -> (NewAxis Link, NewAxis Link)
moveZ (z0, (xn, yn)) (newObj, zIndex, obj) = --obj@(Object (x, y, z) (kx, ky, kz)) = 
    let --newObj      = updateObject obj (xn, yn, ptr)
        --zIndex      = (+) kz . fromMaybe 0 $ takeCoordinate . line $ z
        lineZ       = line z0 
        (zn, ptr)   = (\(a,b) -> (reconstructor z0 a, reconstructor z0 b))
            . (flip insertLink $ Link zIndex [newObj]) 
            . fmap_ (flip removeObject $ obj) $ lineZ
     in (zn, ptr)

