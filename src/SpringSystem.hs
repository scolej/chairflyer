module SpringSystem where

data SpringState =
  SpringState { sPos :: Double
              , sVel :: Double
              , sTime :: Double
              }

data SpringRate =
  SpringRate { srVel :: Double
             , srAcc :: Double
             , srTime :: Double
             }

-- | Simple spring & damper force system.
computeSpringRate :: Double -> SpringState -> SpringRate
computeSpringRate dt s =
  SpringRate { srVel = sVel s
             , srAcc = (0.5 - (sPos s)) * 0.01 - ((sVel s) * 0.1)
             , srTime = 1 -- Time passes at 1 second per second
             }

-- * Boilerplate

sAddRate :: SpringRate -> SpringState -> SpringState
sAddRate r s =
  SpringState { sPos = sPos s + srVel r
              , sVel = sVel s + srAcc r
              , sTime = sTime s + srTime r
              }

sRateScale :: Double -> SpringRate -> SpringRate
sRateScale x r =
  SpringRate { srVel = x * srVel r
             , srAcc = x * srAcc r
             , srTime = x * srTime r
             }

sRateAdd :: SpringRate -> SpringRate -> SpringRate
sRateAdd a b =
  SpringRate { srVel = srVel a + srVel b
             , srAcc = srAcc a + srAcc b
             , srTime = srTime a + srTime b
             }
