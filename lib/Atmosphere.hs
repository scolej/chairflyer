module Atmosphere
  ( isaDensity
  , isaPressure
  , isaTemperature
  ) where

t0, p0, r :: Double
t0 = 288.15
p0 = 101325
r = 287.04

isaDensity :: Double -> Double
isaDensity h = isaPressure h / isaTemperature h / r

isaTemperature :: Double -> Double
isaTemperature h = t0 - 6.5 / 1000 * h

isaPressure :: Double -> Double
isaPressure h = p0 * (1 - 0.0065 * h / t0) ** 5.2561