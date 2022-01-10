{-# LANGUAGE DeriveGeneric #-}

module SimState where

import Data.Aeson
import GHC.Generics (Generic)
import Data.Time
import Data.Ratio

import Atmosphere
import NVector
import Vec

-- | Common simulation state. All aircraft/models must produce these data
-- to plug into the rest of the sim.
data SimState = SimState
  { ssTime     :: UTCTime -- ^ Current time
  , ssAltitude :: Double  -- ^ Metres above sea-level
  , ssPosition :: NVec    -- ^ Position
  , ssVelocity :: Vec2    -- ^ 2D velocity, longitudinal & vertical
  , ssHeadingV :: Vec3    -- ^ Aircraft heading vector
  , ssLanded   :: Bool    -- ^ True indicates we're on the ground
  }
  deriving (Generic)

instance ToJSON SimState
instance FromJSON SimState

-- | Finds the angle from true north of the current heading vector.
ssHeading :: SimState -> Double
ssHeading s = heading (ssPosition s) (ssHeadingV s)

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
  s0 { ssTime = addUTCTime (fromRational $ approxRational dt 1e-4) t0
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
    -- velocity vector, including wind (but only if we're not "landed")
    vv = (vx `scalev3` hv0) `addv3` (if ssLanded s0 then zerov3 else w)
    p1 = unitv3 $ destinationV (unitv3 vv) (dt * magv3 vv) p0
    -- heading vector is always orthogonal to position vector
    hv1 = unitv3 $ crossv3 (crossv3 p1 hv0) p1
