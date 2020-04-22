module AcSystem where

import Debug.Trace
import Atmosphere
import Handy
import Vec
import AcState
import Integrators

-- FIXME maybe I belong in AcState
data PilotInput =
  PilotInput { piThrottle :: Double
             , piPitch :: Double
             }

data AcSystem = 
  AcSystem { sysState :: AcState
           , sysInput :: PilotInput
           -- , sysControllers :: [AcController Double]
           -- Ahhh removing the list here might solve the class dilemma as well!
           -- controllers must cooperate, so there's no sense abstracting their state here
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
  

-- data Controller s i c =
--   Controller { 

-- type Controller s i c = s -> Double -> c -> (i -> i, c)
-- type AcController c = Controller AcState PilotInput c

type ControlStep s i c = s -> i -> Double -> c -> (i, c)

data Controller s i c =
  Controller { cStep :: ControlStep s i c
             , cState :: c
             }

type AcController = Controller AcState PilotInput

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



-- Attempt #1
--
--   -- | A controller.
--   -- s   the system under control
--   -- a   a value extracted from the system which is to be controlled
--   -- i   the system inputs which can be updated by the controller
--   -- c   controller state
--   data Controller s a i c =
--     Controller { cGetInput :: s -> a -- ^ Get the control target value from the system
--                , cGetOutput :: c -> i -> i -- ^ Update the system inputs with the controllers output
--                , cStep :: a -> Double -> c -> c -- ^ Step the controller with a new value and its old state over a timestep
--                }
--   
--   acPitchController :: Double -> Controller AcState Double PilotInputs Double
--   acPitchController targetV = 
--     Controller { cGetInput = magv2 . acVel
--                , cGetOutput = \p pin -> pin { piPitch = p }
--                , cStep v dt p -> if v < targetV
--                                  then p - delta * dt
--                                  else p + delta * dt
--                                  where delta = degToRad 1 / 2 -- Adjust half a degree each second
--                }
