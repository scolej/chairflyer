module SpringSystem where

data SpringState =
  SpringState { sPos :: Double
              , sVel :: Double
              }

data SpringRate =
  SpringRate { srVel :: Double
             , srAcc :: Double
             }

sAddRate :: SpringRate -> SpringState -> SpringState
sAddRate r s =
  SpringState { sPos = sPos s + srVel r
              , sVel = sVel s + srAcc r
              }

sRateScale :: Double -> SpringRate -> SpringRate
sRateScale x r =
  SpringRate { srVel = x * srVel r
             , srAcc = x * srAcc r
             }

sRateAdd :: SpringRate -> SpringRate -> SpringRate
sRateAdd a b =
  SpringRate { srVel = srVel a + srVel b
             , srAcc = srAcc a + srAcc b
             }

computeSpringRate :: Double -> SpringState -> SpringRate
computeSpringRate dt s =
  SpringRate { srVel = sVel s
             , srAcc = -0.01 * (sVel s)
             -- , srAcc = (0.5 - (sPos s)) * 0.1 - ((sVel s) * 0.01)
             }
