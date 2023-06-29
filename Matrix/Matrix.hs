module Matrix where 

import Data.List

type Point = (Int,Int)
type Element = (Char,Point)
type Plan = [[Element]]

show :: Plan -> IO ()
show = putStrLn . intercalate "\n" . map (intersperse ' ' . map fst)

plan :: Int -> Int -> Plan
plan x y = [[(' ',(x,y)) | x <- [-x..x]] | y <- [y,(y-1)..(-y)]]

on :: (Int -> Int) -> Plan -> Plan
on f = map (map g)
    where
    g = \(c,(x,y)) -> if f x == y then ('+',(x,y)) else (c,(x,y))

put :: Element -> Plan -> Plan
put e = map (map f)
    where
    f = \x -> if snd e == snd x then e else x

y :: Element -> Int
y = snd . snd

set :: Element -> Plan -> Plan
set e (p:ps) 
    | y e == (y $ head p) = alloc e p : ps
    | otherwise           = if ps == [] then p:ps else p:set e ps

alloc :: Element -> [Element] -> [Element]
alloc e (x:xs) 
    | snd x == snd e = e:xs
    | otherwise      = if xs == [] then x:xs else x:alloc e xs

get :: Point -> Plan -> Maybe Element
get p (e:[]) = foldl (\acc x -> (snd x == p) ? (Just x,acc)) Nothing e
get p (e:es) = if f e /= Nothing then f e else get p es
    where
    f = foldl (\acc x -> (snd x == p) ? (Just x,acc)) Nothing 

putMap :: [Element] -> Plan -> Plan
putMap (x:[]) = put x 
putMap (x:xs) = putMap xs . put x
    
(?) :: (Read a) => Bool -> (a,a) -> a
(?) True  = fst
(?) False = snd
