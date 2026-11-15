module Virtual where

import qualified Space as Space

-- [!] Form :: (Space.Coord a) => b -> a

data X = X
data Y = Y
data Z = Z

newtype Form a = Form { magnitude :: (Double,Double) }

data Command = Copy 
    { x :: Form X
    , y :: Form Y
    , z :: Form Z
    , o :: Space.Point }

-- [!] Memory:
data Virtual a = Virtual -- Monad.State [?]
    { upload   :: a -> Virtual a 
    , download :: [Command] -> [Virtual a]
    }
    
-- [!] script :: String -> Maybe [Command] -- [!] Outside function


-- State 

-- [!] todo: algorítmo de busca em Grafo

