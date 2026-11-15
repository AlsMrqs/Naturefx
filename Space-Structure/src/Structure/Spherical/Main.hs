module Main where

import qualified System.Process as Process

data Graph a k = Vertex
    { vert :: a
    , edge :: k -> Graph a k } 

cross :: k -> Graph a k -> Graph a k
cross = flip edge

instance (Show a) => Show (Quartic a) where
    show g = 
        "\n"++
        show (vert $ cross W up)++ " " ++(show $ vert up)++ " " ++show (vert $ cross E up)
        ++"\n"++
        show (vert $ cross W g) ++ " " ++ (show $ vert g) ++ " " ++ show (vert $ cross E g)
        ++"\n"++
        show (vert $ cross W down)++ " " ++(show $ vert down)++ " " ++show (vert $ cross E down)
        ++"\n"
        where
        up = cross N g
        down = cross S g

data Dir = N | S | W | E

type Quartic a = Graph a Dir

main :: IO ()
main = navigate (build [1,2,3] [[4,5,6],[7,8,9],[10,11,12]])
    
navigate :: Quartic Int -> IO ()
navigate g = (print g >> getChar) >>= \input -> Process.system "clear" 
    >> case input of
        '8' -> navigate $ cross N g
        '2' -> navigate $ cross S g
        '4' -> navigate $ cross W g
        '6' -> navigate $ cross E g
        _   -> return ()

link :: Quartic a -> Quartic a -> Quartic a -> Quartic a -> (Dir -> Quartic a)
link up down left right = \dir -> case dir of
    N -> up
    S -> down
    W -> left
    E -> right

build :: [a] -> [[a]] -> Quartic a
build (x:xs) ys = node
    where
    node          = Vertex x (link up down left right)
    (up  , down)  = vertical   ys (node, node)
    (left, right) = horizontal xs (node, node)

vertical :: [[a]] -> (Quartic a, Quartic a) -> (Quartic a, Quartic a)
vertical sets (origin, node) = case sets of
    []          -> (,) last origin where last = node
    ((x:xs):ys) -> (,) last next
        where
        next          = Vertex x (link node down left right) 
        (left, right) = horizontal xs (next, next)
        (last, down)  = vertical   ys (origin, next) 

horizontal :: [a] -> (Quartic a, Quartic a) -> (Quartic a, Quartic a)
horizontal set (origin, node) = case set of
    []     -> (,) last origin where last = node
    (x:xs) -> (,) last next
        where
        up            = edge node N
        down          = edge node S
        next          = Vertex x (link (edge up E) (edge down E) node right)
        (last, right) = horizontal xs (origin, next) 

