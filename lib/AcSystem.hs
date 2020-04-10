module AcSystem where

import Vec

-- | Aircraft state variables
data AcState =
  AcState
    { acTime :: Double -- ^ Absolute time
    , acPos :: Vec3 -- ^ Position in 3D space
    , acVel :: Vec2 -- ^ 2D velocity, longitudinal & vertical
    , acMass :: Double -- ^ Mass of aircraft
    , acHeading :: Double -- ^ Aircraft heading
    , acPitch :: Double -- ^ Aircraft pitch
    }

-- | Aircraft rate of change variables
data AcRate =
  AcRate
    { acrTime :: Double -- ^ Rate of change of absolute time; most likely equal 1
    , acrVel :: Vec2 -- ^ 2D velocity, longitudinal & vertical
    , acrAcc :: Vec2 -- ^ 2D acceleration, longitudinal & vertical
    , acrMass :: Double -- ^ Rate of change of mass
    , acrHeading :: Double -- ^ Turn rate
    , acrPitch :: Double -- ^ Pitch rate
    }

-- | Compute a unit vector pointing forward along the aircraft's body
acUnitForward :: AcState -> Vec2
acUnitForward s =
  let p = acPitch s
  in Vec2 (cos p) (sin p)

-- | Compute a unit vector pointing up from the aircraft's body
acUnitUp :: AcState -> Vec2
acUnitUp s =
  let p = acPitch s
  in Vec2 (-sin p) (cos p)

-- | Compute a unit vector pointing backward along the aircraft's body
acUnitBack :: AcState -> Vec2
acUnitBack = scalev2 (-1) . acUnitForward

-- | Compute a unit vector pointing in the reverse direction to the aircraft's motion
acUnitVelForward :: AcState -> Vec2
acUnitVelForward = unitv2 . acVel

-- | Compute a unit vector pointing in the direction of the aircraft's motion
acUnitVelBack :: AcState -> Vec2
acUnitVelBack = scalev2 (-1) . acUnitVelForward

-- | Compute a unit vector pointing upwards at right angles to the aircraft's motion
acUnitVelUp :: AcState -> Vec2
acUnitVelUp s = Vec2 (-y) x
  where Vec2 x y = acUnitVelForward s

-- | Aircraft properties.
data AcProps =
  AcProps
    { acpMass :: Double
    , acpLiftingArea :: Double
    , acpDraggingArea :: Double
    , acpMaxThrust :: Double
    }
  deriving (Show)

hackyJab :: AcProps
hackyJab =
  AcProps
    { acpMass = 540
    , acpLiftingArea = 10
    , acpDraggingArea = 2
    , acpMaxThrust = 1000
    }

-- FIXME should we be using dt somehow?
acRate :: AcProps -> Double -> AcState -> AcRate
acRate props dt s =
  AcRate
    { acrTime = 1
    , acrVel = acVel s
    , acrAcc = (1 / acMass s) `scalev2` sumv2 [weight, lift, drag, thrust]
    , acrMass = 0
    , acrHeading = degToRad 5
    , acrPitch = 0
    }
  where
    v = magv2 (acVel s)
    weight = Vec2 0 (-9.81 * 540)
    -- weight = Vec2 0 (-9.81 * acMass s)
    q = 0.5 * 1.225 * v * v
    lift = (q * cl * (acpLiftingArea props)) `scalev2` acUnitVelUp s
    aoa = radTwixtv2 (acUnitVelForward s) (acUnitForward s)
    ar = 7.4
    cl =
      if abs aoa > degToRad 15
        then 0
        -- then error ":("
        else 2 * pi * ar / (ar + 2) * aoa
    cd = 0.027 + cl * cl / pi / ar / 0.8
    drag = (q * cd * (acpDraggingArea props)) `scalev2` acUnitVelBack s
    thrust = (acpMaxThrust props) `scalev2` acUnitForward s

acClip :: AcState -> AcState
acClip s = s { acPos = p
             , acVel = v
             }
  where Vec3 x y z = acPos s
        underground = z < 0
        Vec2 vx vz = acVel s
        p = Vec3 x y (if underground then 0 else z)
        v = Vec2 vx (if underground then 0 else vz)

acStep :: Double -> AcRate -> AcState -> AcState
acStep dt r s =
  AcState
    { acTime = acTime s + dt
    , acPos = acPos s `addv3` (dt `scalev3` vel3)
    , acVel = acVel s `addv2` (dt `scalev2` acrAcc r )
    , acMass = acMass s + acrMass r
    , acHeading = acHeading s + acrHeading r
    , acPitch = acPitch s + acrPitch r
    }
  where
    Vec2 vx vz = acrVel r
    h = acHeading s
    vel3 = Vec3 (vx * sin h) (vx * cos h) vz -- Hack to propagate 2D velocity through 3D space
