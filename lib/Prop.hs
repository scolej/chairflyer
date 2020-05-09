module Prop where

import Handy

-- Hacky thrust coefficient as a function of advance ratio.
thrustCoeff :: Double -> Double
thrustCoeff = clip 0 1 . (*) 0.10 . cos . (*) (pi / 2 * 1.3)  . clip 0 1

propThrust :: Double -> Double -> Double -> Double -> Double
propThrust d rho n v =
  let j = v / n / d
      kt = thrustCoeff j
  in kt * rho * n ** 2 * d ** 4

rpmToRps :: Double -> Double
rpmToRps r = r / 60
