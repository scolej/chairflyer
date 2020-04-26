module Handy where

lerp :: Double -> Double -> Double -> Double
lerp a b x = (1 - x) * a + x * b

norm :: Double -> Double -> Double -> Double
norm a b x = (x - a) / (b - a)

rescale :: Double -> Double -> Double -> Double -> Double -> Double
rescale a b c d x = lerp c d (norm a b x)

clip :: Double -> Double -> Double -> Double
clip a b x | x < a = a
           | x > b = b
           | otherwise = x

degToRad :: Double -> Double
degToRad = (*) (pi / 180)

radToDeg :: Double -> Double
radToDeg = (*) (180 / pi)
