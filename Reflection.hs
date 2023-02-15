module Reflection where

import System.Environment
import System.IO

import qualified Data.ByteString.Lazy as B
import Data.Functor.Identity
import Data.List

import Control.Monad.Trans.Writer.Lazy
import Control.Monad

main :: IO ()
main = do
    progname <- getProgName
    args <- getArgs
    putStr $ (++"\n") progName
    putStr $ concatMap (++"\n") args

