module Renderer where

import qualified Graphics.Rendering.OpenGL as OpenGL
import qualified Graphics.UI.GLUT as GLUT
import qualified Control.Monad.State as State
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Structure.Graph as Graph
import qualified Structure.BFS as BFS
import qualified Universe as Universe
import qualified Space as Space
import qualified Virtual as Virtual

import qualified Utils.Generator as Generator
import qualified Utils.Conversor as Conversor

-- :: (_ a) => a -> IO ()
-- Generic executor

start :: IO ()
start = do
    (progName,_) <- GLUT.getArgsAndInitialize
    (GLUT.$=) GLUT.initialWindowSize (GLUT.Size 600 600)
    _window <- GLUT.createWindow progName
    entity  <- generateEntity 20
    ((GLUT.$=) GLUT.displayCallback . GLUT.addTimerCallback 120) (displayEntity entity)
    GLUT.mainLoop

generateEntity :: Int -> IO (Universe.Entity Space.Point)
generateEntity = fmap format . Generator.generate
    where 
    format = 
        Universe.Entity 
            (GLUT.Points) 
            (OpenGL.Color3 1 0 (1 :: GLUT.GLfloat))
        . Conversor.build 
        . Conversor.load

-- [!] Distribute:
-- [!] Entity -> Virtual (Map Element (Set Entity)) -- State Virtual 

newtype Element = Element Space.Point -- [!] center of gravity

type Scope a = Map.Map Element (Set.Set (Universe.Entity a))

-- memory :: State.State (Virtual.Virtual (Scope a)) (Scope a)
-- memory = do
--     (Virtual.Virtual k _) <- State.get 
--     return k

-- [!] Virtual : [insert,remove,apply]
-- [!] Input -> (IO (), Map Interface Virtual)

-- [!] (Ord a, Space.Coord a) => Observer a -> 

displayEntity :: (Ord a, Space.Coord a) => Universe.Entity a -> GLUT.DisplayCallback
displayEntity k = do
    GLUT.clear [GLUT.ColorBuffer]
    GLUT.color (OpenGL.Color3 1 0 (1 :: GLUT.GLfloat))
    renderEntity k
    GLUT.swapBuffers
    GLUT.postRedisplay Nothing

-- [#] 
renderEntity :: (Ord a, Space.Coord a) => Universe.Entity a -> IO ()
renderEntity k = do
    GLUT.color (Universe.color k) 
    OpenGL.renderPrimitive 
        (Universe.model k)
        . mapM_ (flash . map Space.point . Map.keys)
        $ BFS.connected (Universe.graph k)
    return ()

-- [!] todo: Render Graph.Graph (Origin Perspective)

flash :: [Space.Point] -> IO ()
flash = mapM_ (\(Space.Point (x,y,z)) -> OpenGL.vertex $ OpenGL.Vertex3 x y z)

