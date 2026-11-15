module Tree where

import Graph.Grammar
import Data.Maybe
--import Graph.Automaton
import Lexer

data Tree a = Node a [Tree a] | Leaf a
    deriving Show

isLeaf :: Tree a -> Bool
isLeaf tree = case tree of
    (Leaf _) = True
    _        = False

--instance Functor Tree where -- to foldl in (f3) maybe...
--    fmap f (Node x y) = Node (f x) y
--    fmap f (Leaf x)   = Leaf (f x)
--
quantum :: Symbol -> Tree Token
quantum symbol = case symbol of
    (Terminal term) -> Leaf []
    (Variable rule) -> Node [] []
--
--f2 :: Production -> Tree Token
--f2 = map (quantum . chain)
--
--f3 :: Token -> Tree Token -> Tree Token
--f3 token tree = case tree of
--    (Node _ subtree) -> Node token subtree
--    (Leaf _) -> Leaf token

f4 :: Tree a -> Maybe [([Tree a] -> Tree a, [Tree a], [Tree a])]
f4 tree = case tree of
    (Leaf _)   -> Nothing 
    (Node x y) -> if null y 
        then Just [(Node x, [], [])]
        else Just [(Node x, [head y], (tail y))]

memory :: Tree a -> Memory -> Maybe Memory
memory tree stack 
    | isLeaf tree && null stack = Nothing
    | 

--f5 :: Tree a -> Memory a -> Either (Tree a) (Memory a)
--f5 tree stack = 
--    if null stack
--        then Left (f4 tree)
--        else 

    -- Memory ??
type Memory = (Maybe Tree a, [ ([Tree a] -> Tree a, [Tree a], [Tree a]) ])

pop :: Memory -> 
pop (e,         []) = if isLeaf e then Left [] else Right 
pop (e, (n,l,r):xs) =

alloc
        
free :: Memory -> Maybe Memory
free (e ,stack) = 
    if isNothing e || (isLeaf e && null stack)
        then Nothing
        else
            

push :: Maybe (Tree a) -> Memory -> Maybe Memory
push tree stack = if null stack
    then (tree, stack)
    else 
        if 
