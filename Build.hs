module Build where

import System.Environment
import System.IO
import Data.List

data Tree a = Leaf a | Node a (Tree a) (Tree a)

-- Data.Base --
html = intercalate "\n" source
source = 
    ["<!DOCTYPE html>"
    ,"<html>"
    ,"<head>" ,"<title>","</title>" ,"</head>"
    ,"<body>","</body>"
    ,"</html>"]
-- End --

genesis = getArgs >>= mapM_ (`System.IO.writeFile` html)

-- Todo --
-- tree :: (Foldable f) => f [Char] -> Tree [Char]
-- seed :: [Char] -> Tree [Char]
