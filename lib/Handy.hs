module Handy where

import Data.List

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

-- | Create a piecewise defined function which linearly interpolates
-- between the given points. If the input exceeds the bounds of the
-- supplied points, the boundary value is returned. The supplied
-- points must be sorted on their first value.
piecewiseLerp :: [(Double, Double)] -> Double -> Double
piecewiseLerp ps x = go lp rp
  where lp = find (\(z, _) -> z < x) (reverse ps)
        rp = find (\(z, _) -> z >= x) ps
        go (Just (l, a)) (Just (r, b)) = lerp a b ((x - l) / (r - l))
        go Nothing (Just (_, b)) = b
        go (Just (_, a)) Nothing = a
        go Nothing Nothing = 0
