module Controller where

data Controller s = Controller (Double -> s -> (s, Controller s))

idController :: Controller s
idController = Controller (\_ s -> (s, idController))

pidController
  :: (Double, Double, Double) -- ^ PID constants
  -> (s -> Double) -- ^ How to extract the signal from the state
  -> (Double -> s -> s) -- ^ How to update the state with the controller's output
  -> Double -- ^ Target value
  -> Controller s
pidController (kp, ki, kd) f g target = go pidZero
  where
    go c0 = Controller
      (\dt s0 ->
         let t0 = pidTime c0
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
         in (s, go c))

-- FIXME could hide the constants, g, f, target... in PIDState
-- Maybe that would clean up this mess a bit.
-- Also provides possibility that controller could change its own target!

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

-- meanDErr :: [(Double, Double)] -> Double
-- meanDErr ((y,):ss) = mean $ go 0 ss
--   where go t (x:xs) =
-- meanDErr _ = 0

mean :: [Double] -> Double
mean [] = 0
mean xs = sum xs / fromIntegral (length xs)
