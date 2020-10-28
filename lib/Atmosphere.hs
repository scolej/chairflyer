module Atmosphere
  ( isaDensity
  , isaPressure
  , isaTemperature
  , Atmosphere(..)
  , isa
  , isaWind
  , constSeaLevel
  , rAir
  , gammaAir
  ) where

import Vec
import NVector

data Atmosphere =
  Atmosphere
  { atmosDensity     :: Double
  , atmosTemperature :: Double
  , atmosPressure    :: Double
  , atmosWind        :: Vec3
  }

-- | Constant sea-level conditions with no wind.
constSeaLevel :: Double -> Atmosphere
constSeaLevel _ =
  Atmosphere
  { atmosDensity     = d0
  , atmosTemperature = t0
  , atmosPressure    = p0
  , atmosWind        = zerov3
  }

-- | International Standard Atmosphere with no wind.
isa :: NVec -> Double -> Atmosphere
isa _ h =
  Atmosphere
  { atmosDensity     = isaDensity h
  , atmosTemperature = isaTemperature h
  , atmosPressure    = isaPressure h
  , atmosWind        = zerov3
  }

-- | International Standard Atmosphere with a wind function.
isaWind
  :: (NVec -> Vec3) -- ^ Wind as function of position
  -> NVec           -- ^ Position
  -> Double         -- ^ Height
  -> Atmosphere     -- ^ Atmospheric conditions
isaWind w p h =
  (isa p h) { atmosWind = w p }

t0, p0, d0, rAir, gammaAir :: Double
t0 = 288.15
p0 = 101325
d0 = isaDensity 0
rAir = 287.04
gammaAir = 1.4

isaDensity :: Double -> Double
isaDensity h = isaPressure h / isaTemperature h / rAir

isaTemperature :: Double -> Double
isaTemperature h = t0 - 6.5 / 1000 * h

isaPressure :: Double -> Double
isaPressure h = p0 * (1 - 0.0065 * h / t0) ** 5.2561
