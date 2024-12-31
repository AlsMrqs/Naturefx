module Main where

import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL
import System.IO
import System.Process
import Control.Concurrent
import Data.Time.Clock
import Data.Bool

import Space
import Axis

line0 =
    map (\r -> (0.0, 0.0,  r)) [0, 5e-2 .. 0.5] ++
    map (\r -> (0.0, 0.0, -r)) [0, 5e-2 .. 0.5] ++
    map (\r -> (0.0,  r, 0.0)) [0, 5e-2 .. 0.5] ++
    map (\r -> (0.0, -r, 0.0)) [0, 5e-2 .. 0.5] ++
    map (\r -> (r,  0.0, 0.0)) [0, 5e-2 .. 0.5] ++
    map (\r -> (-r, 0.0, 0.0)) [0, 5e-2 .. 0.5] ++ [(0.5,0.5,0.5)] 

rotateScheme diff = flip rotateZ (1*diff) . flip rotateY (1*diff) . flip rotateX 0.5

axisRotation :: Double -> DisplayCallback
axisRotation t0 = do
    diff <- getTime >>= return . (\x -> x-t0)
    clear [ColorBuffer]
    renderPrimitive Points $ mapM_ (draw . pointToGLPoint . rotateScheme diff) line0
    swapBuffers    >> postRedisplay Nothing
    system "clear" >> print diff

main :: IO ()
main = do
    (progName,_) <- getArgsAndInitialize
    _window      <- createWindow "GLUT - learning" 
    currentTime  <- getTime
    displayCallback       $= addTimerCallback 16 (axisRotation currentTime)
    keyboardMouseCallback $= Just (keyboardMouse)
    mainLoop

keyboardMouse :: KeyboardMouseCallback
keyboardMouse key keyState _ _ = do
    case (key, keyState) of
        (Char 'q', Down) -> leaveMainLoop
        (Char ' ', Down) -> putStrLn "space!"
        _                -> return ()

draw :: (GLfloat,GLfloat,GLfloat) -> DisplayCallback
draw = \(x,y,z) -> vertex $ Vertex3 x y z

pointToGLPoint :: Point -> (GLfloat,GLfloat,GLfloat)
pointToGLPoint (x,y,z) = 
    (realToFrac(x), realToFrac(y), realToFrac(z)) :: (GLfloat,GLfloat,GLfloat)

