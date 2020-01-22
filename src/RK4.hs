module RK4 where

import Vec

-- | Aircraft state variables
data AcState =
  AcState { acTime :: Double -- ^ Absolute time
          , acPos :: Vec3 -- ^ Position in 3D space
          , acVel :: Vec2 -- ^ 2D velocity, longitudinal & vertical
          , acMass :: Double -- ^ Mass of aircraft
          , acHeading :: Double -- ^ Aircraft heading
          , acPitch :: Double -- ^ Aircraft pitch
          }

-- | Aircraft rate of change variables
data AcRate =
  AcRate { acrTime :: Double -- ^ Rate of change of absolute time; most likely equal 1
         , acrVel :: Vec2 -- ^ 2D velocity, longitudinal & vertical
         , acrAcc :: Vec2 -- ^ 2D acceleration, longitudinal & vertical
         , acrMass :: Double -- ^ Rate of change of mass
         , acrHeading :: Double -- ^ Turn rate
         , acrPitch :: Double -- ^ Pitch rate
         }

computeAcRates :: Double -> AcState -> AcRate
computeAcRates dt s =
  AcRate { acrTime = acTime s + dt
         , acrVel = acVel s
         , acrAcc = foldl addv2 zerov2 [weight, lift, drag, thrust]
         , acrMass = 0
         , acrHeading = 0
         , acrPitch = 0
         }
  where weight = Vec2 0 (-9.81 * acMass s)
        lift = Vec2 0 0
        drag = Vec2 0 0
        thrust = Vec2 0 0

-- | How to add rate variables to state variables
acAddRate :: AcRate -> AcState -> AcState
acAddRate r s =
  AcState { acTime = acTime s + acrTime r
          , acPos = acPos s `addv3` vel3
          , acVel = acVel s `addv2` acrVel r
          , acMass = acMass s + acrMass r
          , acHeading = acHeading s + acrHeading r
          , acPitch = acPitch s + acrPitch r
          }
  where p = acPitch s
        h = acHeading s
        v = magv2 (acVel s)
        -- Hack to propagate 2D velocity through 3D space
        vel3 = Vec3 (v * cos p * sin h) (v * cos p * cos h) (v * sin p)

-- | How to scale the sytem change rate with a scalar value
acScaleRate :: Double -> AcRate -> AcRate
acScaleRate x r =
  AcRate { acrTime = x * acrTime r
         , acrVel = x `scalev2` acrVel r
         , acrAcc = x `scalev2` acrAcc r
         , acrMass = x * acrMass r
         , acrHeading = x * acrHeading r
         , acrPitch = x * acrPitch r
         }

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
