module Space.Particle.Particle where

import Control.Exception

data ParticleException = ParticleException String 
    deriving (Show)

instance Exception ParticleException

data Particle = Particle Int Int Int
    deriving (Eq,Show)

proton (Particle p _ _) = p
neutron (Particle _ n _) = n
electron (Particle _ _ e) = e

type Proton   = Int
type Neutron  = Int
type Electron = Int

newParticle :: Proton -> Neutron -> Electron -> IO Particle
newParticle p n e 
    | any (<0) [p,n,e] = 
        let exception = "Particle cannot have a negative number of elements!" ++
                        "(newParticle "++(show p)++" "++(show n)++" "++(show e)++")"
         in throw (ParticleException exception)
    | all (==0) [p,n,e] =
        let exception = "Particle cannot be completely empty!" ++
                        "(newParticle "++(show p)++" "++(show n)++" "++(show e)++")"
         in throw (ParticleException exception)
    | otherwise = return (Particle p n e)

charge :: Particle -> Double
charge x = 
    let charge = (fromIntegral $ proton x) - (fromIntegral $ electron x)
     in charge * 16.02176634e-19

isIon :: Particle -> Bool
isIon (Particle p _ e) = p - e /= 0

particleMass :: Particle -> Double
particleMass (Particle p n e) = 
    let protonMass   = fromIntegral(p) * 1.672e-27
        neutronMass  = fromIntegral(n) * 1.672e-27
        electronMass = fromIntegral(e) * 9.109e-31
     in (protonMass + neutronMass + electronMass)

