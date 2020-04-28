module AcSystem where

import AcState
import Controller
import Handy
import Integrators
import Vec

data AcSystem =
  AcSystem { sysState :: AcState
           , sysController :: AcController
           }

stepAcSystem :: Double -> AcSystem -> AcSystem
stepAcSystem dt sys =
  sys { sysState = acClip . rk4step jabRate acStep dt $ ss' }
  where
    c = sysController sys
    ss = sysState sys
    (ss', _) = (cStep c) ss dt (cState c)

-- | Controller for aircraft state which keeps its own state as a double.
type AcController = Controller AcState Double

-- | Really dodgy airspeed controller.
-- If we're going too fast pitch up!
-- If we're going too slow pitch down!
airspeedController :: Double -> ControlStep AcState Double
airspeedController targetV s dt _ = (s', 0)
  where
    p = acPitch s
    v = magv2 . acVel $ s
    delta = degToRad 0.05
    Vec3 _ _ z = acPos s
    onGround = z < 0.5
    pMax = degToRad 15
    pMin = if onGround then 0 else degToRad (-5)
    dv = v - targetV
    p' = clip pMin pMax $ p + dv * delta * dt
    s' = s { acPitch = p' }
