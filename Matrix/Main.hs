module Main where

import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL

import System.IO
import System.Process
import Control.Concurrent

import Axis
import Data.Bool
import Data.Time.Clock

--getStraight (x,y,z) =
--    let angle = getAngle 

getTime :: IO Double --Integer
getTime = do
    current <- return . diffTimeToPicoseconds . utctDayTime =<< getCurrentTime
    return $ fromIntegral(current) / 1e12

line1 = map pointToGLPoint line0
line0 = 
    --map (\r -> ((*) r $ cos (5*pi/4), (*) r $ sin (5*pi/4), 0)) [0, 1e-2 .. 0.5]  ++
    --map (\r -> ((*) r $ cos (3*pi/4), (*) r $ sin (3*pi/4), 0)) [0, 1e-2 .. 0.5]  ++
    --map (\r -> ((*) r $ cos (7*pi/4), (*) r $ sin (7*pi/4), 0)) [0, 1e-2 .. 0.5]  ++
    --map (\r -> ((*) r $ cos (pi/4), (*) r $ sin (pi/4), 0)) [0, 1e-2 .. 0.5] ++
    map (\r -> (0.0, 0.0, r)) [0, 1e-2 .. 0.5] ++
    map (\r -> (0.0, 0.0, -r)) [0, 1e-2 .. 0.5] ++
    map (\r -> (0.0, r, 0.0)) [0, 1e-2 .. 0.5]  ++
    map (\r -> (0.0, -r, 0.0)) [0, 1e-2 .. 0.5] ++
    map (\r -> (r, 0.0, 0.0)) [0, 1e-2 .. 0.5]  ++
    map (\r -> (-r, 0.0, 0.0)) [0, 1e-2 .. 0.5] 

main :: IO ()
main = do
    (progName,_) <- getArgsAndInitialize
    _window      <- createWindow "GLUT - learning" 
    currentTime <- getTime
    displayCallback $= addTimerCallback 30 (display currentTime)
    keyboardMouseCallback $= Just (keyboardMouse)
    mainLoop

display t0 = do
    t <- getTime
    let diff = ((t-1) - t0)
    clear [ColorBuffer]
    --renderPrimitive Points $ draw $ pointToGLPoint $ p0
    renderPrimitive Points $ mapM_ draw $ 
        map pointToGLPoint $ --line0
        map (\p -> rotateY p diff) $ --line0-- $ 
        map (\p -> rotateZ p diff) $ --line0 -- $
        map (\p -> rotateX p diff) line0
    swapBuffers
    postRedisplay Nothing
    system "clear"
    print diff

pointToGLPoint :: Point -> (GLfloat,GLfloat,GLfloat)
pointToGLPoint (x,y,z) = 
    (realToFrac(x)::GLfloat, realToFrac(y)::GLfloat, realToFrac(z)::GLfloat)

keyboardMouse :: KeyboardMouseCallback
keyboardMouse key keyState _ _ = do
    case (key, keyState) of
        (Char 'q', Down) -> leaveMainLoop
        (Char ' ', Down) -> putStrLn "space!"
        _                -> return ()

draw :: (GLfloat,GLfloat,GLfloat) -> DisplayCallback
draw = \(x,y,z) -> vertex $ Vertex3 x y z

