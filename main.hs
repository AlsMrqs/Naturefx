import System.Directory
import System.Environment
import System.IO.Error
import System.IO

import Build

main :: IO ()
main = do
    genesis
    putStr $ (++"\n") "lambda-x!"

