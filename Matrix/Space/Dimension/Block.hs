module Block where

data Line a = Point (Line a) a (Line a)
            | Void

instance (Show a) => Show (Line a) where
    show Void = ""
    show (Point l x r) = show (takePoint $ Point l x r)

instance Functor Line where
    fmap _ Void = Void
    fmap f (Point l x r) = Point (fmap f l) (f x) (fmap f r)

takePoint Void          = [] 
takePoint (Point l x r) = (takePointL l) ++ [x] ++ (takePointR r)
    where
        takePointL Void          = []
        takePointL (Point l x _) = (takePointL l) ++ [x]
        takePointR Void          = []
        takePointR (Point _ x r) = [x] ++ (takePointR r)

insert :: (Eq a, Ord a) => Line a -> a -> Line a
insert s@(Void)        x = Point (Void) x (Void)
insert s@(Point l y r) x 
    | x == y = Point l y r
    | x >  y = Point l y (linkR r (s, x))
    | x <  y = Point (linkL l (s, x)) y r

linkR :: (Eq a, Ord a) => Line a -> (Line a, a) -> Line a
linkR s@(Void)        (last, x) = Point (last) x (Void)
linkR s@(Point l y r) (last, x) 
    | x == y = Point l y r
    | x >  y = Point l y (linkR r (s, x))
    | x <  y = Point l x (linkR r (s, y))

linkL :: (Eq a, Ord a) => Line a -> (Line a, a) -> Line a
linkL s@(Void)        (last, x) = Point (Void) x (last)
linkL s@(Point l y r) (last, x) 
    | x == y = Point l y r
    | x <  y = Point (linkL l (s, x)) y r
    | x >  y = Point (linkL l (s, y)) x r

-- [1] (a -><- b)

