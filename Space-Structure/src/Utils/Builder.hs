module Utils.Builder where

import qualified Graphics.Rendering.OpenGL as OpenGL
import qualified Graphics.UI.GLUT as GLUT

import qualified Universe as Universe
import qualified Space as Space

import qualified Utils.Generator as Generator
import qualified Utils.Conversor as Conversor

generateEntity :: Int -> IO (Universe.Entity Space.Point)
generateEntity = fmap format . Generator.generate
    where 
    format = 
        Universe.Entity 
            (GLUT.Points) 
            (OpenGL.Color3 1 1 (1 :: GLUT.GLfloat))
        . Conversor.build 
        . Conversor.load

