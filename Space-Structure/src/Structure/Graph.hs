module Structure.Graph where

import qualified Data.Map as Map

type Constellation a = [Star a]
type Star a = (a,[a])

type Graph a = Map.Map a (Vertex a)
data Vertex a = Vertex { label :: a, children :: [Vertex a] } 

instance Show a => Show (Vertex a) where
    show v = "Vertex { label = "++(strLabel v)++", children = "++(strChildren v)++" }"
        where
        strLabel    = show . Structure.Graph.label
        strChildren = show . map Structure.Graph.label . Structure.Graph.children

