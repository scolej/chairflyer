module AcState where

import Atmosphere
import Handy
import Vec
import LiftDrag

-- | Aircraft state variables
data AcState =
  AcState
    { acTime :: Double -- ^ Absolute time
    , acPos :: Vec3 -- ^ Position in 3D space
    , acVel :: Vec2 -- ^ 2D velocity, longitudinal & vertical
    , acMass :: Double -- ^ Mass of aircraft
    , acHeading :: Double -- ^ Aircraft heading, radians
    , acPitch :: Double -- ^ Aircraft pitch, radians
    , acThrottle :: Double -- ^ Throttle setting. FIXME I feel out of place.
    }

-- | Aircraft rate of change variables
data AcRate =
  AcRate
    { acrVel :: Vec2 -- ^ 2D velocity, longitudinal & vertical
    , acrAcc :: Vec2 -- ^ 2D acceleration, longitudinal & vertical
    , acrMass :: Double -- ^ Rate of change of mass
    }

-- | Aircraft properties.
data AcProps =
  AcProps
    { acpMass :: Double
    , acpLiftingArea :: Double
    , acpDraggingArea :: Double
    , acpMaxThrust :: Double
    }
  deriving (Show)

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

-- | Make a rough estimate of how much thrust is available.
-- Nothing about this is correct.
-- As velocity increases thrust should go to 0: fixed pitch propellor performs worse and worse.
-- As density decreases thrust should go to 0: less oxygen to burn, less density to push.
-- Don't go below zero.
hackyThrustAvailable :: Double -> Double -> Double -> Double
hackyThrustAvailable maxThrust density v = clip 0 maxThrust t
  where a = rescale 0 120 1 0 v
        b = rescale 1.225 0.9 1 0 density
        t = maxThrust * a * b

-- FIXME if we don't use dt, is there any advantage to using RK4?
acRate :: (Double -> Atmosphere) -> AcProps -> Double -> AcState -> AcRate
acRate atmos props _ s =
  AcRate
    { acrVel = acVel s
    , acrAcc = (1 / acMass s) `scalev2` sumv2 [weight, lift, drag, thrustv]
    , acrMass = 0
    }
  where
    v = magv2 (acVel s)
    weight = Vec2 0 (-9.81 * acMass s)
    Vec3 _ _ h = acPos s
    onGround = h < 0.1
    rho = (atmosDensity . atmos) h
    q = 0.5 * rho * v * v
    lift = (q * cl * (acpLiftingArea props)) `scalev2` acUnitVelUp s
    aoa = alpha s
    ar = 7.4
    (cl, cd) = liftDrag ar aoa
    groundDrag = if onGround then 400 else 0 -- TODO Could be better.
    dragMag = (q * cd * (acpDraggingArea props)) + groundDrag
    drag = dragMag`scalev2` acUnitVelBack s -- TODO Could be better, ground drag is not in the same direction.
    thrust = acThrottle s * hackyThrustAvailable (acpMaxThrust props) rho v
    thrustv = thrust `scalev2` acUnitForward s

-- | Compute angle of attack.
alpha :: AcState -> Double
alpha s = radTwixtv2 (acUnitVelForward s) (acUnitForward s)

-- | Make sure we can't fly underground.
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
  s { acTime = acTime s + dt
    , acPos = acPos s `addv3` (dt `scalev3` vel3)
    , acVel = acVel s `addv2` (dt `scalev2` acrAcc r )
    , acMass = acMass s + acrMass r
    }
  where
    Vec2 vx vz = acrVel r
    h = acHeading s
    -- Hack to propagate 2D velocity through 3D space.
    -- Need to worry about great circles & rhumb lines.
    vel3 = Vec3 (vx * sin h) (vx * cos h) vz

hackyJab :: AcProps
hackyJab =
  AcProps
    { acpMass = 540
    , acpLiftingArea = 10
    , acpDraggingArea = 2
    , acpMaxThrust = 1000
    }

-- | Rate of change function for the hacky Jabiru in standard atmosphere.
jabRate :: Double -> AcState -> AcRate
jabRate = acRate isa hackyJab
