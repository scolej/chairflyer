module NVector where

import Vec
import Handy

earthRadius :: Double
earthRadius = 6378137

-- See:
-- A Non-singular Horizontal PositionRepresentation
-- Kenneth Gade
-- THE JOURNAL OF NAVIGATION
-- 2010

type NVec = Vec3

-- Latitude & longitude in radians or degrees.
-- FIXME Use an angle type you fool.
type LatLon = (Double, Double)

llToNVec :: (Double, Double) -> NVec
llToNVec (lat, lon) = Vec3 (sin lat)
                           (sin lon * cos lat)
                           (-1 * cos lon * cos lat)

nvecToLL :: NVec -> (Double, Double)
nvecToLL (Vec3 x y z) = ( atan2 x (sqrt $ y ** 2 + z ** 2)
                        , atan2 y (-z)
                        )

llRadToDeg, llDegToRad :: (Double, Double) -> (Double, Double)
llRadToDeg (lat, lon) = (radToDeg lat, radToDeg lon)
llDegToRad (lat, lon) = (degToRad lat, degToRad lon)

nvecToLLDeg :: NVec -> LatLon
nvecToLLDeg = llRadToDeg . nvecToLL

llDegToNVec :: LatLon -> NVec
llDegToNVec = llToNVec . llDegToRad

destination :: NVec -> Double -> Double -> NVec
destination s h dist = (cos t `scalev3` s) `addv3` (sin t `scalev3` d)
  where t = dist / earthRadius
        d = (cos h `scalev3` n) `addv3` (sin h `scalev3` e)
        e = unitv3 $ Vec3 1 0 0 `crossv3` s
        n = s `crossv3` e
