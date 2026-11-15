module Main where

import Control.Concurrent   (threadDelay)
import System.Process       (system)
import System.IO            (BufferMode(NoBuffering), hSetBuffering, stdout, hSetEcho, stdin, hGetChar, hReady)

import Data.List            (intercalate)

import Plane

data Cycle = Empty | Node { getPoint :: (Int,Int) , left :: Cycle , right :: Cycle }

instance Show Cycle where
    show cycle =
        let center = show . getPoint $ cycle
            left1  = show . getPoint $ left cycle
            left2  = show . getPoint . left $ left cycle
            left3  = show . getPoint . left . left $ left cycle
            right1 = show . getPoint $ right cycle
            right2 = show . getPoint . right $ right cycle
            right3 = show . getPoint . right . right $ right cycle
        in left3 ++" "++ left2 ++" "++ left1 ++" "++  center ++" "++  right1 ++" "++  right2 ++" "++ right3

interface :: Cycle -> [String]
interface cycle =
        let center = show . getPoint $ cycle
            left1  = show . getPoint $ left cycle
            left2  = show . getPoint . left $ left cycle
            left3  = show . getPoint . left . left . left $ cycle
            right1 = show . getPoint $ right cycle
            right2 = show . getPoint . right $ right cycle
            right3 = show . getPoint . right . right . right $ cycle
        in [left3, left2, left1, center, right1, right2, right3]

main :: IO ()
main = do
    hSetBuffering stdin  NoBuffering
    hSetBuffering stdout NoBuffering
    putStrLn "\ESC[?25l"

    let plane = newPlane (3,3)
        (xPerimeter,yPerimeter) = circumference plane

    start (xPerimeter, yPerimeter)
    putStrLn "\ESC[?25h"

start :: (Cycle, Cycle) -> IO ()
start (horizontal, vertical) = do
    frame $ "\n" ++ ((++) "\t\t\t" . intercalate "\n\t\t\t" $ reverse $ interface vertical) ++"\n\n  "++ (show horizontal) 
    input <- getChar
    case input of
        '4' -> start (turn (-1) horizontal, vertical)
        '6' -> start (turn ( 1) horizontal, vertical)
        '2' -> start (horizontal, turn (-1) vertical)
        '8' -> start (horizontal, turn ( 1) vertical)
        _   -> return ()

turn :: Int -> Cycle -> Cycle
turn direction cycle = 
    case direction of
        ( 1) -> right cycle
        (-1) -> left cycle

clearBuffer :: IO ()
clearBuffer = do
    ready <- hReady stdin
    if ready 
        then hGetChar stdin >> clearBuffer
        else return ()

frame :: String -> IO ()
frame str = do
    system "clear"
    putStrLn str

circumference :: Plane -> (Cycle,Cycle)
circumference plane =
    let x = genPoints . (*) 4 . (\n -> (n*2)+1) . fst $ limit plane
        y = genPoints . (*) 4 . (\n -> (n*2)+1) . snd $ limit plane
    in (newCycle x, newCycle y)

genPoints :: Int -> [(Int,Int)]
genPoints last = (:) (0,0) $ zip [(-last+1),(-last+2)..(last)] [1..(last-1)]

newCycle :: [(Int,Int)] -> Cycle
newCycle []     = Empty
newCycle (p:ps) = n0
    where
        n0 = Node p solidLastOne nextNode
        (solidLastOne, nextNode) = betweenLast ps n0 n0

        betweenLast :: [(Int,Int)] -> Cycle -> Cycle -> (Cycle,Cycle)
        betweenLast []     lastNode headNode = (lastNode, headNode)
        betweenLast [p]    lastNode headNode = let n2 = Node p lastNode headNode in (n2, n2)
        betweenLast (p:ps) lastNode headNode = (theLast, n1)
            where
                n1 = Node p lastNode nextNode
                (theLast, nextNode) = betweenLast ps n1 headNode

