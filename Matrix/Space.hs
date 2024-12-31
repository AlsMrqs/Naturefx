module Space where

import Particle
import Axis
import Data.Bool
import Data.Time.Clock

type Space a = [Element a]
type Time    = Double

data Element a = Element a Mass Time Point
    deriving (Eq,Show)

element (Element x _ _ _) = x
mass    (Element _ x _ _) = x
time    (Element _ _ x _) = x
point   (Element _ _ _ x) = x

type Mass = Double

getTime :: IO Double 
getTime = do
    currentTime <- getCurrentTime
    return 
        . (\x -> (fromIntegral x) / 1e12) 
        . diffTimeToPicoseconds $ utctDayTime currentTime

