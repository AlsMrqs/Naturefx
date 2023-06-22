import Data.List

data Point = Point (Int,Int)
data Object = Object Char Point
data Board = Board [[Object]]

instance Show Board where
    show (Board l) = intercalate "\n" . map (intersperse ' ') $ map (map f) l
        where
        f (Object c _) = c

newBoard :: Int -> Int -> Board
newBoard x y = Board [[Object ' ' (Point (x,y)) | x <- [-x..x]] | y <- [y,y-1..(-y)]]

putObject :: Char -> Point -> Board -> Board
putObject c (Point (x,y)) (Board b) = Board $ map (map f) b
    where
    f (Object c' (Point (x',y')))
        | x' == x && y' == y = Object c (Point (x,y))
        | otherwise          = Object c' (Point (x',y'))

onBoard :: Board -> IO ()
onBoard b = do
    input <- getLine
    let p = read input :: (Int,Int)
        bx = putObject '+' (Point p) b
    print bx
    onBoard bx

main :: IO () 
main = onBoard $ newBoard 10 10
