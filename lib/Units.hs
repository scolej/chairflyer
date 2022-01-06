module Units where

mToFt :: Double -> Double
mToFt = (*) 3.28084

ftToM :: Double -> Double
ftToM x = x / 3.28084

mToNm :: Double -> Double
mToNm = (*) 0.0005399565

mpsToFpm :: Double -> Double
mpsToFpm mps = mToFt mps * 60

fpmToMps :: Double -> Double
fpmToMps fpm = ftToM fpm / 60

knotsToMps :: Double -> Double
knotsToMps k = k / 1.944

mpsToKnots :: Double -> Double
mpsToKnots = (*) 1.944

hourlyToPerSecond :: Double -> Double
hourlyToPerSecond x = x / 60 / 60
