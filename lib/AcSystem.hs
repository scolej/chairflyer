module AcSystem where

import AcState
import Atmosphere
import Handy
import Integrators
import Vec

-- FIXME maybe I belong in AcState
data PilotInput =
  PilotInput { piThrottle :: Double
             , piPitch :: Double
             }

data AcSystem =
  AcSystem { sysState :: AcState
           , sysInput :: PilotInput
           , sysController :: AcController Double
           }

stepAcSystem :: Double -> AcSystem -> AcSystem
stepAcSystem dt s = s { sysState = ss'
                      , sysInput = i'
                      , sysController = c''
                      }
  where
    c = sysController s
    i = sysInput s
    (i', c') = (cStep . sysController $ s)
                  (sysState s) i dt (cState . sysController $ s)
    c'' = c { cState  = c' } -- FIXME nasty, something is wrong here
    ss' = acClip . rk4step jabRate acStep dt $ (sysState s) { acPitch = c' } -- FIXME crappy

-- | A function representing a controller.
type ControlStep s i c = s -> i -> Double -> c -> (i, c)

-- | A controller with a stepping function and some state.
data Controller s i c =
  Controller { cStep :: ControlStep s i c
             , cState :: c
             }

type AcController = Controller AcState PilotInput

-- | Really dodgy airspeed controller.
-- If we're going too fast pitch up!
-- If we're going too slow pitch down!
airspeedController :: Double -> ControlStep AcState PilotInput Double
airspeedController targetV s i dt _ = (i', p'')
  where
    p = acPitch s
    v = magv2 . acVel $ s
    delta = degToRad 0.05
    Vec3 _ _ z = acPos s
    onGround = z < 0.5
    pMax = degToRad 15
    pMin = if onGround then 0 else degToRad (-5)
    dv = v - targetV
    p' = p + dv * delta * dt
    p'' = clip pMin pMax p'
    i' = i { piPitch = p'' }
