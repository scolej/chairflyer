module Prop where

import Handy
import Atmosphere

-- | Hacky thrust coefficient.
thrustCoeff
  :: Double -- ^ Reynolds number
  -> Double -- ^ Propellor tip mach number
  -> Double -- ^ Advance ratio
  -> Double -- ^ Thrust coefficient
thrustCoeff re mtip j = 0.12 * a * b * c
  where a = 1 -- FIXMEEEE no Reynolds effect for the moment
        b = piecewiseLerp [(0, 1), (0.8, 0.8), (1, 0)] mtip
        c = cos . (*) (pi / 2) . clip 0 1 $ j

propThrust
  :: Double -- ^ Propellor diameter
  -> Double -- ^ Air density
  -> Double -- ^ Revolutions per second
  -> Double -- ^ Freestream velocity
  -> Double -- ^ Propellor thrust
propThrust d rho n v =
  let j = v / n / d
      kt = thrustCoeff 1 (mTip n d v 288) j -- FIXME
  in kt * rho * n ** 2 * d ** 4

rpmToRps :: Double -> Double
rpmToRps r = r / 60

mTip
  :: Double -- ^ Revolutions per second
  -> Double -- ^ Propellor diameter
  -> Double -- ^ Freestream velocity
  -> Double -- ^ Air temperature, Kelvin
  -> Double -- ^ Propellor tip Mach number
mTip n d v t = (sqrt $ u ** 2 + v ** 2) / a
  where u = n * pi * d
        a = sqrt $ gammaAir * rAir * t

rey
  :: Double -- ^ Air density
  -> Double -- ^ Freestream velocity
  -> Double -- ^ Characteristic length
  -> Double -- ^ Viscosity
  -> Double -- ^ Reynold's number
rey rho v d mu = rho * v * d / mu
