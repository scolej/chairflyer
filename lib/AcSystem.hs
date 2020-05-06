module AcSystem where

import AcState
import Controller
import Handy
import Integrators
import Vec

data AcSystem =
  AcSystem { sysState :: AcState
           , sysController :: Controller AcState
           }

-- FIXME lens?
updateState :: (AcState -> AcState) -> AcSystem -> AcSystem
updateState f ac =
  let acs = sysState ac
  in ac { sysState = f acs }

stepAcSystem :: Double -> AcSystem -> AcSystem
stepAcSystem dt sys0 =
  AcSystem { sysState = s
           , sysController = c
           }
  where
    Controller c0 = sysController sys0
    s0 = sysState sys0
    (s1, c) = c0 dt s0
    s = acClip . rk4step jabRate acStep dt $ s1

-- | Really dodgy airspeed controller.
-- If we're going too fast pitch up!
-- If we're going too slow pitch down!
airspeedController :: Double -> Controller AcState
airspeedController targetV = Controller $
  \dt s0 -> let p = acPitch s0
                v = magv2 . acVel $ s0
                delta = degToRad 0.05
                z = acAltitude s0
                onGround = z < 0.5
                pMax = degToRad 15
                pMin = if onGround then 0 else degToRad (-5)
                dv = v - targetV
                p' = clip pMin pMax $ p + dv * delta * dt
                s = s0 { acPitch = p' }
            in (s, airspeedController targetV)
