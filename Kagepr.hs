module Kagepr where

import System.Environment
import System.IO
import qualified Data.List as L
import Data.Foldable

data Tree a = Root | Leaf a | Node a (Tree a) (Tree a) deriving Show

instance Functor Tree where
    fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)
    fmap f (Leaf a) = Leaf (f a)
    fmap f _ = Root

-- Data.Base --
_html_ = L.intercalate "\n" source
source = 
    ["<!DOCTYPE html>"
    ,"<html>"
    ,"<head>" ,"<title>","</title>" ,"</head>"
    ,"<body>","</body>"
    ,"</html>"]
-- End --

genesis = getArgs >>= mapM_ (`System.IO.writeFile` _html_)

tag x = "<" ++ x ++ ">"

page = html (Node "1" (Kagepr.head Root) (body Root))

html x = Node "<html>" (x) (Leaf "</html>")
body x = Node "<body>" (x) (Leaf "</body>")
head x = Node "<head>" (x) (Leaf "</head>")

-- Todo --
--kernel :: Tree [Char] -> Tree [Char] 
--kernel = (`Node "<!DOCTYPE html>"` Root) . ((`Node "<html>"` (Leaf "</html>"))

-- Todo --
-- tree :: (Functor f) => f [Char] -> Tree [Char]
-- seed :: [Char] -> Tree [Char]

