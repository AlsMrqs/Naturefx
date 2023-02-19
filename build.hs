module Lambda where

html = intercalate "\n" source
source = 
    ["<!DOCTYPE html>"
    ,"<html>"
    ,"<head>"
    ,"<title>","</title>"
    ,"</head>"
    ,"<body>","</body>"
    ,"</html>"]

genesis :: String -> IO ()
genesis = getArgs

main :: IO ()
main = do 
    getArgs >>= mapM_ (`System.IO.writeFile` html) 


