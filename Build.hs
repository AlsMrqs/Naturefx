module Build where

import System.Environment
import System.IO
import Data.List
import Data.Foldable

data Tree a = Root | Leaf a | Node a (Tree a) (Tree a)

instance Functor Tree where
    fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)
    fmap f (Leaf a) = Leaf (f a)
    fmap f _ = Root

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
