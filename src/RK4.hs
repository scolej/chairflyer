module RK4 where

import Vec

-- System variables
-- Vec3, Vec2, Double
-- just need (* :: Double -> b -> b) and (+ :: b -> b -> b)
-- Is this num?
-- Then the problems are:
--   - if we use a list, they can be different lengths
--   - list must have homogenous elements, so need a sum type of state var types

-- | Aircraft state variables
data AcSystem =
  AcSystem { acTime :: Double -- ^ Absolute time
           , acPos :: Vec3 -- ^ Position in 3D space
           , acVel :: Vec2 -- ^ 2D velocity, longitudinal & vertical
           , acMass :: Double -- ^ Mass of aircraft
           }

-- | Aircraft rate of change variables
data AcRate =
  AcRate { acrTime :: Double -- ^ Rate of change of absolute time; most likely equal 1
         , acrVel :: Vec2 -- ^ 2D velocity, longitudinal & vertical
         , acrAcc :: Vec2 -- ^ 2D acceleration, longitudinal & vertical
         , acrMass :: Double -- ^ Rate of change of mass
         }

computeAcRates :: Double -> AcSystem -> AcRate
computeAcRates dt s =
  AcRate { acrTime = acTime s + dt
         , acrVel = acVel s
         , acrAcc = foldl addv2 zerov2 [weight, lift, drag, thrust]
         , acrMass = 0
         }
  where weight = Vec2 0 (-9.81 * acMass s)
        lift = Vec2 0 0
        drag = Vec2 0 0
        thrust = Vec2 0 0

rk4step ::
     (b -> a -> a) -- ^ How to add the rate of change type and the state type
  -> (Double -> a -> b) -- ^ How to compute the instantaneous rate of change of the system.
  -> (Double -> b -> b) -- ^ How to scale a rate of change
  -> (b -> b -> b) -- ^ How to add rates of change
  -> Double -- ^ Time step
  -> a -- ^ Initial state
  -> a -- ^ Next state
rk4step sa f rs ra dt s0 = sa (rs (dt / 6) r) s0
  where r = foldl1 ra [f1, rs 2 f2, rs 2 f3, f4]
        f1 = f 0 s0
        f2 = f (dt / 2) (sa (rs (dt / 2) f1) s0)
        f3 = f (dt / 2) (sa (rs (dt / 2) f2) s0)
        f4 = f dt (sa (rs dt f3) s0)

type Pos2 = Vec2

type Vel2 = Vec2

type Acc2 = Vec2
