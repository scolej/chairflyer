module Jabiru
  ( jabiru
  , hackyJab
  ) where

import AcState
import NVector
import Atmosphere
import Integrators

hackyJab :: AcProps
hackyJab =
  AcProps
  { acpMass        = 540
  , acpLiftingArea = 6.9
  , acpMaxPropRpm  = 3100
  , acpPropD       = 1.524
  }

-- FIXME atmosphere is not consistent between rate & step!

-- | Rate of change function for the hacky Jabiru in standard atmosphere.
jabRate :: Double -> AcState -> AcRate
jabRate = acRate isa hackyJab

jabiru
  :: (NVec ->
      Double ->
      Atmosphere) -- ^ Atmosphere
  -> Double       -- ^ Time step
  -> AcState      -- ^ Start state
  -> AcState      -- ^ Next state
jabiru atmos dt =
  acClip . rk4step jabRate (acStep atmos) dt
