module Dimension where

data Tree a = Node (Tree a) a (Tree a) | Void deriving Show

node :: (Eq a) => Tree a -> Maybe a
node Void         = Nothing
node (Node _ a _) = Just a 

find :: (Eq a) => a -> Tree a -> Tree a
find _ Void = Void
find x a | pure x == node a = a
         | otherwise        = 

type Number = Double

data Axis = X Number [(Tree Axis, Tree Axis)]
          | Y Number [(Tree Axis, Tree Axis)]
          | Z Number [(Tree Axis, Tree Axis)] 
          deriving Show

malloc :: Tree Axis -> (Double, Double, Double) -> (Tree Axis, Tree Axis, Tree Axis)
malloc Void (x,y,z) = (x0, y0, z0)
    where
        x0 = Node Void (X x [(y0, z0)]) Void
        y0 = Node Void (Y y [(x0, z0)]) Void
        z0 = Node Void (Z z [(x0, y0)]) Void
malloc (Node l (X n plan) r) (x,y,z) =
    if x == n 
        then
            let (y0, z0) = plan
            if node y0 == pure y
                then 
                else
        else
            
malloc (Node l (Y n plan) r) (x,y,z) =
    if y == n then
              else
malloc (Node l (Z n plan) r) (x,y,z) =
    if z == n then
              else

