module Kagepr where

import System.IO
import System.Environment

import Prelude hiding (head,sort)
import qualified Data.List as List

data Tree a = Root | Leaf a | Node a (Tree a) (Tree a) deriving Show

instance Functor Tree where
    fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)
    fmap f (Leaf a) = Leaf (f a)
    fmap f _ = Root

genesis = getArgs >>= mapM_ (`System.IO.writeFile` _page_)

_page_ = _html_
_html_ = sort . html $ 
    Node "\n    https://url.info\n" 
        ((head . Leaf) "\n    ...\n") 
        ((body . Leaf) "\n    ...\n")

-- Generic --
seed :: [a] -> Tree [a] -> Tree [a]
seed x = \n -> Node x n Root

sort :: Tree [a] -> [a]
sort Root         = []
sort (Leaf x)     = x 
sort (Node x l r) = (++) x ((++) (sort l) (sort r))

-- Working on Three --
html = \x -> (Node "<html>") (x) (Leaf "</html>")
body = \x -> (Node "<body>") (x) (Leaf "</body>")
head = \x -> (Node "<head>") (x) (Leaf "</head>")

url :: [Char] -> Tree [Char]
url = (`seed` (Leaf "index.html"))

