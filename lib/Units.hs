module Units where

mToFt :: Double -> Double
mToFt = (*) 3.28084

mToNm :: Double -> Double
mToNm = (*) 0.0005399565

mpsToFpm :: Double -> Double
mpsToFpm mps = mToFt mps * 60

mpsToKnots :: Double -> Double
mpsToKnots = (*) 1.944
