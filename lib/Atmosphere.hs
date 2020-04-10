module Atmosphere
  ( isaDensity
  , isaPressure
  , isaTemperature
  , Atmosphere(..)
  , isa
  , constSeaLevel
  ) where

data Atmosphere =
  Atmosphere { atmosDensity :: Double
             , atmosTemperature :: Double
             , atmosPressure :: Double
             }

constSeaLevel :: Double -> Atmosphere
constSeaLevel _ =
  Atmosphere { atmosDensity = d0
             , atmosTemperature = t0
             , atmosPressure = p0
             }

isa :: Double -> Atmosphere
isa h = Atmosphere { atmosDensity = isaDensity h
                   , atmosTemperature = isaTemperature h
                   , atmosPressure = isaPressure h
                   }

t0, p0, d0, r :: Double
t0 = 288.15
p0 = 101325
d0 = isaDensity 0
r = 287.04

isaDensity :: Double -> Double
isaDensity h = isaPressure h / isaTemperature h / r

isaTemperature :: Double -> Double
isaTemperature h = t0 - 6.5 / 1000 * h

isaPressure :: Double -> Double
isaPressure h = p0 * (1 - 0.0065 * h / t0) ** 5.2561