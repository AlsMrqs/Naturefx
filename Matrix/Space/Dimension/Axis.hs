module Axis where

import Line

data Axis a = X (Line a)
            | Y (Line a) 
            | Z (Line a)

instance (Show a) => Show (Axis a) where
    show (X line) = "X " ++ (show line)
    show (Y line) = "Y " ++ (show line)
    show (Z line) = "Z " ++ (show line)

instance (Eq a) => Eq (Axis a) where
    (==) (X Void)          (X Void)          = True
    (==) (X (Point _ a _)) (X (Void))        = False
    (==) (X (Point _ a _)) (X (Point _ b _)) = (a == b)

    (==) (Y Void)          (Y Void)          = True
    (==) (Y (Point _ a _)) (Y (Void))        = False
    (==) (Y (Point _ a _)) (Y (Point _ b _)) = (a == b)

    (==) (Z Void)          (Z Void)          = True
    (==) (Z (Point _ a _)) (Z (Void))        = False
    (==) (Z (Point _ a _)) (Z (Point _ b _)) = (a == b)

line :: Axis a -> Line a
line axis = case axis of
    X ln -> ln
    Y ln -> ln
    Z ln -> ln

reconstructor :: Axis a -> Line a -> Axis a
reconstructor (X _) = X
reconstructor (Y _) = Y
reconstructor (Z _) = Z

-- X (Point _ (Link Double [Object () (Double, Double, Double)]) _)
