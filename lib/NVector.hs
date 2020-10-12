module NVector where

import Vec
import Handy
import Data.Fixed

-- Earth radius, metres
earthRadius :: Double
earthRadius = 6378137

-- See:
-- A Non-singular Horizontal PositionRepresentation
-- Kenneth Gade
-- THE JOURNAL OF NAVIGATION
-- 2010

-- Unit vector, normal to the surface at the position it represents.
-- Everything here is spherical only.
-- An n-vector should always have unit length.
-- x: positive towards north pole
type NVec = Vec3

northPole :: NVec
northPole = Vec3 1 0 0

--
--
--

-- Latitude & longitude in radians or degrees.
type LatLon = (Double, Double)

llToNVec :: (Double, Double) -> NVec
llToNVec (lat, lon) =
  Vec3 (sin lat)
       (sin lon * cos lat)
       (-1 * cos lon * cos lat)

nvecToLL :: NVec -> (Double, Double)
nvecToLL (Vec3 x y z) =
  ( atan2 x (sqrt $ y ** 2 + z ** 2)
  , atan2 y (-z)
  )

llRadToDeg, llDegToRad :: (Double, Double) -> (Double, Double)
llRadToDeg (lat, lon) = (radToDeg lat, radToDeg lon)
llDegToRad (lat, lon) = (degToRad lat, degToRad lon)

nvecToLLDeg :: NVec -> LatLon
nvecToLLDeg = llRadToDeg . nvecToLL

llDegToNVec :: LatLon -> NVec
llDegToNVec = llToNVec . llDegToRad

--
--
--

destination :: NVec -> Double -> Double -> NVec
destination s h dist = (cos t `scalev3` s) `addv3` (sin t `scalev3` d)
  where t = dist / earthRadius
        d = (cos h `scalev3` n) `addv3` (sin h `scalev3` e)
        e = unitv3 $ northPole `crossv3` s
        n = s `crossv3` e

destinationV
  :: NVec   -- ^ Initial positions
  -> Vec3   -- ^ Heading vector, unit length
  -> Double -- ^ Distance to travel, metres
  -> NVec   -- ^ Final position
destinationV p v d =
  v

--
--
--

-- | Wrap an angle into the range 0 to 2 pi.
wrapHeading :: Double -> Double
wrapHeading h = h `mod'` (2 * pi)

initialHeading :: NVec -> NVec -> Double
initialHeading a b =
  let c = crossv3 a b
      d = crossv3 a northPole
  in wrapHeading $ radTwixtv3 a d c
