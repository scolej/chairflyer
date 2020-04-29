module SpringSystem2 where

data SpringState =
  SpringState { sPos :: Double
              , sVel :: Double
              , sTime :: Double
              , sForce :: Double
              }

data SpringRate =
  SpringRate { srVel :: Double
             , srAcc :: Double
             }

-- | Simple spring & damper force system.
springRate :: Double -> SpringState -> SpringRate
springRate _ s =
  SpringRate { srVel = sVel s
             , srAcc = (0.5 - sPos s) * 0.01 - (sVel s * 0.1) + sForce s
             }

springStep :: Double -> SpringRate -> SpringState -> SpringState
springStep dt r s =
  s { sPos = sPos s + srVel r * dt
    , sVel = sVel s + srAcc r  * dt
    , sTime = sTime s + dt
    }
