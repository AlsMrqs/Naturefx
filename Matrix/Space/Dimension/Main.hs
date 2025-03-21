module Main where

import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Parallel -- .Strategies
import Data.Time.Clock
import Data.Bool            (bool)

import Space
import Axis
import Test
import Control
import qualified Universe as Universe

main :: IO ()
main = do 
    tid0 <- forkIO timer
    tid1 <- forkIO (getTime >>= timerTest)
    getChar >>= print . bool False True . (==) 'q'
    mapM_ killThread [tid0, tid1]

timer :: IO ()
timer = threadDelay 1000000 >> putStrLn "Count!" >> timer

timerTest :: Double -> IO ()
timerTest t = do
    current <- getTime
    if floor t == floor current
        then do
            threadDelay 10000
            timerTest t
        else do 
            print current
            timerTest current

--main :: IO ()
--main = do
--    (progName,_) <- getArgsAndInitialize
--    _window      <- createWindow "GLUT - learning" 
--    currentTime  <- getTime
--
--    objects      <- genObject 600
--    mvarObjects   <- newMVar objects 
--
--    rotation     <- newMVar $ Mouse (0,0) (0,0) (0,0)
--
--    windowSize            $= Size 600 500
--    displayCallback       $= addTimerCallback 30 (render mvarObjects rotation currentTime)
--    keyboardMouseCallback $= Just (keyboardMouse)
--    motionCallback        $= Just (draggingHandler rotation)
--    mouseCallback         $= Just (mouseHandler rotation)
--    mainLoop

redim_ :: Point -> Point
redim_ (x,y,z) =
    let f = 3.01
        factor  = 1 + (z/f)
        newX = x * factor
        newY = y * factor
     in (newX, newY, z)

render :: MVar [Universe.Object] -> MVar Mouse -> Double -> DisplayCallback
render mvarObj mvarMouse t = do
    diff  <- getTime
    obj   <- readMVar mvarObj
    mouse <- readMVar mvarMouse
    let (x,y) = getMemory mouse
        obj00 = Universe.Object (0,0,0) $ Universe.Properties (0.01,0,0) ""
    clear [ColorBuffer]
    color $ Color3 1.0 1.0 (1.0 :: GLfloat)
    renderPrimitive Points $
        mapM_ ( draw . pointToGLPoint . redim_ . updateView (x,y)) 
            $ map Universe.point obj
    color $ Color3 0.0 0.0 (1.0 :: GLfloat)
    renderPrimitive Points $
        mapM_ ( draw . pointToGLPoint . redim_ . updateView (x,y)) 
            $ map Universe.point $ (:[]) $ head obj
    color $ Color3 1.0 0.0 (0.0 :: GLfloat)
    renderPrimitive Lines $
        mapM_ ( draw . pointToGLPoint . redim_ . updateView (x,y)) 
            $ [(0.5,0.5,0.5),  (0.5,0.5,-0.5)
              ,(0.5,0.5,-0.5), (-0.5,0.5,-0.5)
              ,(-0.5,0.5,-0.5),(-0.5,0.5,0.5)
              ,(-0.5,0.5,0.5), (0.5,0.5,0.5)]
    color $ Color3 1.0 0.0 (0.0 :: GLfloat)
    renderPrimitive Lines $
        mapM_ ( draw . pointToGLPoint . redim_ . updateView (x,y)) 
            $ [(0.5, -0.5,0.5), (0.5, -0.5,-0.5)
              ,(0.5, -0.5,-0.5),(-0.5,-0.5,-0.5)
              ,(-0.5,-0.5,-0.5),(-0.5,-0.5,0.5)
              ,(-0.5,-0.5,0.5), (0.5, -0.5,0.5)]
    obj_ <- takeMVar mvarObj
    putMVar mvarObj $! (map Universe.move obj_)
    swapBuffers >> postRedisplay Nothing

updateView :: (Double,Double) -> Point -> Point
updateView (x,y) = flip rotateY y . flip rotateX x

draw :: (GLfloat,GLfloat,GLfloat) -> DisplayCallback
draw = \(x,y,z) -> vertex $ Vertex3 x y z

pointToGLPoint :: Point -> (GLfloat,GLfloat,GLfloat)
pointToGLPoint (x,y,z) = (realToFrac(x), realToFrac(y), realToFrac(z)) :: (GLfloat,GLfloat,GLfloat)


