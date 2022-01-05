{-# LANGUAGE DeriveGeneric #-}

module SimState where

import Data.Binary
import GHC.Generics (Generic)

import Atmosphere
import NVector
import Vec

-- | Common simulation state. All aircraft/models must produce these data
-- to plug into the rest of the sim.
data SimState = SimState
  { ssTime     :: Double -- ^ Total elapsed seconds
  , ssAltitude :: Double -- ^ Metres above sea-level
  , ssPosition :: NVec   -- ^ Position
  , ssVelocity :: Vec2   -- ^ 2D velocity, longitudinal & vertical
  , ssHeadingV :: Vec3   -- ^ Aircraft heading vector
  }
  deriving (Generic)

instance Binary SimState

-- | Step sim state linearly over a time delta.
-- FIXME need to take into account altitude: rescale vx to surface speed
simStep
  :: (NVec ->
      Double ->
      Atmosphere) -- ^ Atmospheric conditions as a function of position
  -> Double       -- ^ Delta time, seconds
  -> SimState     -- ^ Start state
  -> SimState     -- ^ End state
simStep atmos dt s0 =
  s0 { ssTime = t0 + dt
     , ssAltitude = z0 + dt * vz
     , ssPosition = p1
     , ssHeadingV = hv1
    }
  where
    t0 = ssTime s0
    p0 = ssPosition s0
    z0 = ssAltitude s0
    Vec2 vx vz = ssVelocity s0
    w = inPlaneAt p0 $ atmosWind $ atmos p0 z0
    hv0 = ssHeadingV s0
    -- velocity vector, including wind
    vv = (vx `scalev3` hv0) `addv3` w
    p1 = unitv3 $ destinationV (unitv3 vv) (dt * magv3 vv) p0
    -- heading vector is always orthogonal to position vector
    hv1 = unitv3 $ crossv3 (crossv3 p1 hv0) p1
