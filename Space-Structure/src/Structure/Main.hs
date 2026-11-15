module Main where

import qualified Spherical.Toroid as Toroid
import qualified Data.Char as Char

main :: IO ()
main = do
    n1 <- getLine 

toBin :: Int -> [Int]
toBin 1 = [1]
toBin 0 = [0]
toBin x = mod x 2 : toBin (div x 2)

permute :: [a] -> [[a]]
permute [] = []
permute xs = aux xs (length xs)
    where
    aux (x:xs) 0 = []
    aux (x:xs) n = (x:xs) : aux (xs ++ [x]) (n-1)

