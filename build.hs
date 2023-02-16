import System.IO

import Data.List

index = intercalate "\n" source
source = 
    ["<!DOCTYPE html>"
    ,"<html>"
    ,"<head>"
    ,"<title>","</title>"
    ,"</head>"
    ,"<body>","</body>"
    ,"</html>"]

main :: IO ()
main = System.IO.writeFile "index.html" index

