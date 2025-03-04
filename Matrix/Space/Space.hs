module Space.Space where

import Space.Axis
import Space.Particle.Particle
import Data.Bool
import Data.Time.Clock

type Space a = [a]
type Time    = Double
type Mass    = Double

    -- Element (_) -> {Metadata Constructor} --
data Element a = Element 
    { element :: a
    , mass    :: Mass
    , time    :: Time
    , point   :: Point
    } deriving (Eq,Show)

hydrogen  = Particle 1 0 1
helium    = Particle 2 2 2
carbon    = Particle 6 6 6
oxigen    = Particle 8 8 8
elementH  = Element hydrogen (particleMass hydrogen) 0.0 (0,0,-1)
elementHe = Element helium   (particleMass helium)   0.0 (0,0,1)
elementO  = Element oxigen   (particleMass oxigen)   0.0 (0,0,0.5)
elementC  = Element carbon   (particleMass carbon)   0.0 (0,0,0.1)

getTime :: IO Double 
getTime = do
    currentTime <- getCurrentTime
    return 
        . (\x -> (fromIntegral x) / 1e12) 
        . diffTimeToPicoseconds $ utctDayTime currentTime

