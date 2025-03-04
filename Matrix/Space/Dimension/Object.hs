module Object where

import Link
import Axis

data Object = Object (Axis Link, Axis Link, Axis Link) (Double, Double, Double) 

instance Show Object where
    show (Object (a,b,c) (x,y,z)) = "Object " ++ show (xPoint a, yPoint b, zPoint c) ++ show (x,y,z)

instance Eq Object where
    (==) (Object (a,b,c) _) (Object (x,y,z) _) = (a == x) && (b == y) && (c == z)

xPoint = takeIndex . takeLine
yPoint = takeIndex . takeLine
zPoint = takeIndex . takeLine

updateObject :: Object -> (Axis Link, Axis Link, Axis Link) -> Object
updateObject (Object _ (x, y, z)) (xn, yn, zn) = Object (xn, yn, zn) (x, y, z)

--updateObject :: Object -> (Axis a, Axis a, Axis a) -> Object
--updateObject (Object _ (x, y, z)) (xn, yn, zn) = Object (xn, yn, zn) (x, y, z)

obj_0 = Object (x0,y0,z0) (1,1,1)
x0 = X $ Point Void (Link 0 [obj_0]) Void
y0 = Y $ Point Void (Link 0 [obj_0]) Void
z0 = Z $ Point Void (Link 0 [obj_0]) Void

neo :: (Axis Link, Axis Link, Axis Link) -> Object -> (Axis Link, Axis Link, Axis Link)
neo (x0,y0,z0) obj =
    let (x1, newObj) = move (x0, (y1, z1)) obj
        (y1, _     ) = move (y0, (x1, z1)) obj
        (z1, _     ) = move (z0, (x1, y1)) obj
    in  (x1,y1,z1)

type NewAxis a = Axis a
type NewObject = Object

    -- todo
--move :: (Axis a, (NewAxis a, NewAxis a)) -> Object -> (NewAxis a, NewObject)
move :: (Axis Link, (NewAxis Link, NewAxis Link)) -> Object -> (NewAxis Link, NewObject)
move (x0, (yn, zn)) obj@(Object (x, y, z) (kx, ky, kz)) = 
    let newObj      = updateObject obj (xn, yn, zn)
        xIndex      = (+) kx . fromMaybe 0 $ xPoint x
        lineX       = takeLine x0 
        -- fake values -- 
        xn          = (reconstructor x0) . fmap (flip insertObject $ (newObj, xIndex)) . fmap (flip removeObject $ obj) $ lineX
     in (xn, newObj)

--wave :: (Axis a, NewAxisdate a, NewAxisdate a) -> (Axis a, NewAxisdate a)
--wave (x0, yn, zn) =
--        
--
--lineUpdate :: Line a -> Line a
--lineUpdate Void          = Void
--lineUpdate (Point l x r) = 
--    let 

--
--data Energy = Energy 
--    { dataName :: [Char]
--    , kinetics :: (Double, Double, Double) }
--
--k100 = Energy "AlsMrqs!" (1,0,0)
--
--x0 = X (Point Void (Link 0 [ ((y0, z0), k100) ])) Void)
--y0 = Y (Point Void (Link (0, [ ((x0, z0), k100) ])) Void)
--z0 = Z (Point Void (Link (0, [ ((x0, y0), k100) ])) Void)
--

--insert_ :: (Line X, Line Y, Line Z) -> Object a -> (Double,Double,Double) -> (Line X, Line Y, Line Z)
--insert_ (x0,y0,z0) obj (x,y,z) = 

