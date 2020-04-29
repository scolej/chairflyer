module Controller where

-- | A function representing a controller.
-- 's' the state of system under control
-- 'c' the state of the controller itsself
type ControlStep s c = s -> Double -> c -> (s, c)

-- | A controller with a stepping function and some state.
data Controller s c =
  Controller { cStep :: ControlStep s c
             , cState :: c
             }

-- | A controller which does absolutely nothing.
idController :: Controller s Double
idController =
  Controller { cStep = \s _ _ -> (s, 0)
             , cState = 0
             }

data PIDState =
  PIDState { pidErrSum :: Double -- ^ Sum of past error
           , pidPrevErr :: Double -- ^ Previous error
           , pidTime :: Double
           }

pidZero :: PIDState
pidZero =
  PIDState { pidErrSum = 0
           , pidPrevErr = 0
           , pidTime = 0
           }

pidStep
  :: (Double, Double, Double) -- ^ PID constants
  -> (s -> Double) -- ^ How to extract the signal from the state
  -> (Double -> s -> s) -- ^ How to update the state with the controller's output
  -> Double -- ^ Target value
  -> ControlStep s PIDState
pidStep (kp, ki, kd) f g target s0 dt c0 = (s, c)
  where t0 = pidTime c0
        errs0 = pidErrSum c0
        err0 = pidPrevErr c0
        t = t0 + dt
        sig = f s0
        err = target - sig
        errs = errs0 + err * dt
        dd = (err - err0) / dt
        x = kp * err + ki * errs + kd * dd
        c = PIDState { pidErrSum = errs
                     , pidPrevErr = err
                     , pidTime = t
                     }
        s = g x s0

-- meanDErr :: [(Double, Double)] -> Double
-- meanDErr ((y,):ss) = mean $ go 0 ss
--   where go t (x:xs) =
-- meanDErr _ = 0

mean :: [Double] -> Double
mean [] = 0
mean xs = sum xs / fromIntegral (length xs)

pidController
  :: (Double, Double, Double) -- ^ PID constants
  -> (s -> Double) -- ^ How to extract the signal from the state
  -> (Double -> s -> s) -- ^ How to update the state with the controller's output
  -> Double -- ^ Target value
  -> Controller s PIDState
pidController ks f g target =
  Controller { cStep = pidStep ks f g target
             , cState = pidZero
             }
