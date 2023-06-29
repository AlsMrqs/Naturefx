module Kagepr where

import qualified Data.List as List
import System.IO
import System.Environment
import Prelude hiding (head,sort)

-- Data Structures --

data Tree a = Node a (Tree a) (Tree a) | Leaf a | Root 
    deriving Show

-- Instances --

instance Functor Tree where
    fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)
    fmap f (Leaf x)     = Leaf (f x)
    fmap f _            = Root

-- Functions --

genesis = getArgs >>= mapM_ (`System.IO.writeFile` _html_)

merge :: Tree [a] -> [a]
merge (Node x l r) = (++) x ((++) (merge l) (merge r))
merge (Leaf x)     = x 
merge Root         = []

{- remake  (v) -}
_html_ = merge . html $ 
    Node "\n    https://url.info\n" 
        ( head $ Leaf "\n   [..]\n" ) 
        ( body $ Leaf "\n   [..]\n" )

html x = Node "<html>" x (Leaf "</html>")
head x = Node "<head>" x (Leaf "</head>")
body x = Node "<body>" x (Leaf "</body>")

url = (`seed` (Leaf "index.html"))
seed x = \n -> Node x n Root
