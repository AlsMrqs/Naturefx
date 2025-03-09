module Main where

import qualified DBMS as DBMS
import Axis
import Line
import Universe

import System.Random

main :: IO ()
main = do
    let database = DBMS.Empty
        x0       = X $ Node (Coord 0 []) Void Void
        y0       = Y $ Node (Coord 0 []) Void Void
        z0       = Z $ Node (Coord 0 []) Void Void
        space    = Space (x0, y0, z0)
    return ()

genDatabase :: [Object] -> ([Object], DBMS.Tree Object)
genDatabase = foldl (\(lst, db) x -> 
    let (true, dbup) = DBMS.insert x db
     in if true
        then (lst, dbup)
        else (x:lst, dbup)) ([], DBMS.Empty)

genObject :: Int -> IO [Object]
genObject n = sequence . take n . cycle $ [randObject] 
    
randDouble :: IO Double
randDouble = newStdGen >>= return . fst . random

randObject :: IO Object
randObject = do
    x  <- randDouble
    y  <- randDouble
    z  <- randDouble
    kx <- randDouble
    ky <- randDouble
    kz <- randDouble
    return . Object (x,y,z) $ Properties (kx,ky,kz) ""
