module Main where

import Data.Bool

main :: IO ()
main = do
    input <- getChar
    if input == 'q'
        then return ()
        else main
X []
Y []
Z []




getChar >>= return . ((==) 'q') >>= bool main (return ())

