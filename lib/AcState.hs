{-# LANGUAGE DeriveGeneric #-}

module AcState where

import Data.Binary
import GHC.Generics (Generic)
import Atmosphere
import LiftDrag
import NVector
import Prop
import Vec

-- FIXME
-- We seem to steady out WAY too fast.
-- Is airspeed indicator wrong?
-- Or is model wrong? Need tests for basic performance cases.

-- FIXME
reStd
  :: Double -- ^ Velocity
  -> Double -- ^ Altitude, metres
  -> Double -- ^ Reynolds number
reStd v _  = rho * v * d / mu
 where mu = 18.5e-6 -- FIXME double check
       rho = 1
       d = 1
--
--
--

-- | Aircraft state variables
data AcState =
  AcState
    { acTime :: Double -- ^ Absolute time
    , acAltitude :: Double -- ^ Height above sea level, metres
    , acTrack :: (NVec, Double, Double) -- ^ Position, heading, distance travelled
    , acVel :: Vec2 -- ^ 2D velocity, longitudinal & vertical
    , acMass :: Double -- ^ Mass of aircraft
    , acHeading :: Double -- ^ Aircraft heading, radians
    , acPitch :: Double -- ^ Aircraft pitch, radians
    , acThrottle :: Double -- ^ Throttle setting. FIXME I feel out of place.
    }
  deriving (Generic)

instance Binary AcState

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
    , acpMaxPropRpm :: Double
    , acpPropD :: Double
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
    h = acAltitude s
    onGround = h < 0.1
    rho = (atmosDensity . atmos) h
    q = 0.5 * rho * v * v
    lift = (q * cl * acpLiftingArea props) `scalev2` acUnitVelUp s
    aoa = alpha s
    ar = 7.4
    (cl, cd) = liftDrag ar aoa
    groundDrag = if onGround then 20 else 0 -- TODO Could be better.
    dragMag = (q * cd * acpLiftingArea props) + groundDrag
    drag = dragMag `scalev2` acUnitVelBack s -- TODO Could be better, ground drag is not in the same direction.
    rpm = acThrottle s * acpMaxPropRpm props
    thrust = propThrust (acpPropD props) rho (rpmToRps rpm) v
    thrustv = thrust `scalev2` acUnitForward s

-- | Compute angle of attack.
alpha :: AcState -> Double
alpha s = radTwixtv2 (acUnitVelForward s) (acUnitForward s)

-- | Make sure we can't fly underground.
acClip :: AcState -> AcState
acClip s = s { acAltitude = p
             , acVel = v
             }
  where z = acAltitude s
        underground = z < 0
        Vec2 vx vz = acVel s
        p = if underground then 0 else z
        v = Vec2 vx (if underground then 0 else vz)

acStep :: Double -> AcRate -> AcState -> AcState
acStep dt r s =
  s { acTime = acTime s + dt
    , acAltitude = acAltitude s + dt * vz
    -- FIXME need to take into account altitude: rescale vx to surface speed
    , acTrack = (p0, h, d0 + vx * dt)
    , acVel = acVel s `addv2` (dt `scalev2` acrAcc r )
    , acMass = acMass s + acrMass r
    }
  where
    Vec2 vx vz = acrVel r
    (p0, h, d0) = acTrack s

hackyJab :: AcProps
hackyJab =
  AcProps
    { acpMass = 540
    , acpLiftingArea = 6.9
    , acpMaxPropRpm = 3100
    , acpPropD = 1.524
    }

-- | Rate of change function for the hacky Jabiru in standard atmosphere.
jabRate :: Double -> AcState -> AcRate
jabRate = acRate isa hackyJab
