import System.Environment
import System.IO

import Data.List

html = intercalate "\n" source
source = 
    ["<!DOCTYPE html>"
    ,"<html>"
    ,"<head>"
    ,"<title>","</title>"
    ,"</head>"
    ,"<body>","</body>"
    ,"</html>"]

main :: IO ()
main = do
    pages <- getArgs
    mapM_ (\page -> System.IO.writeFile page html) pages

