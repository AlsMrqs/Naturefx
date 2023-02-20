module Kagepr where

import System.Environment
import System.IO
import Data.Foldable
import qualified Data.List as List
import Prelude hiding (head)

data Tree a = Root | Leaf a | Node a (Tree a) (Tree a) deriving Show

instance Functor Tree where
    fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)
    fmap f (Leaf a) = Leaf (f a)
    fmap f _ = Root

-- Data.Base --
_html_ = List.intercalate "\n" source
source = 
    ["<!DOCTYPE html>"
    ,"<html>"
    ,"<head>" ,"<title>","</title>" ,"</head>"
    ,"<body>","</body>"
    ,"</html>"]
-- End --

genesis = getArgs >>= mapM_ (`System.IO.writeFile` _html_)

_page_ = page $ html $ Node "\n" (head $ Leaf "\n...\n") $ body (Leaf "\n...\n")

-- Working on Three --
html x = (Node "<html>") x (Leaf "</html>")
body x = (Node "<body>") x (Leaf "</body>")
head x = (Node "<head>") x (Leaf "</head>")

seed :: [Char] -> (Tree [Char] -> Tree [Char])
seed = (\x n -> Node x n Root)

page :: Tree [Char] -> [Char]
page Root = ""
page (Leaf a) = a
page (Node x l r) = x ++ (page l) ++ (page r)

