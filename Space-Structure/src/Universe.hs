module Universe where

import qualified Graphics.Rendering.OpenGL as OpenGL
import qualified Graphics.UI.GLUT as GLUT

import qualified Structure.Graph as Graph
import qualified Structure.BFS as BFS

-- data Entity a = Entity GLUT.PrimitiveMode (GLUT.Color3 GLUT.GLfloat) (Graph.Graph a)

data Entity a = Entity
    { model :: GLUT.PrimitiveMode       -- geometry
    , color :: GLUT.Color3 GLUT.GLfloat
    , graph :: Graph.Graph a }

-- [!] Element [:] objects builded by Entity's

