module Launcher where

import qualified Graphics.Rendering.OpenGL as OpenGL
import qualified Graphics.UI.GLUT as GLUT

import qualified Utils.Builder

import qualified Renderer

start :: IO ()
start = do
    (progName,_) <- GLUT.getArgsAndInitialize
    (GLUT.$=) GLUT.initialWindowSize (GLUT.Size 600 600)
    _window <- GLUT.createWindow progName
    entity  <- Utils.Builder.generateEntity 20 -- [!] test
    ((GLUT.$=) GLUT.displayCallback . GLUT.addTimerCallback 120) 
        (Renderer.displayEntity entity)
    GLUT.mainLoop

