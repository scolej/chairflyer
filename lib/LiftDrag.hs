module LiftDrag where

import Handy

-- FIXME I'm not linked to ac yet!

liftDrag :: Double -> Double -> (Double, Double)
liftDrag ar aoa = (cl, cd)
  where
    radStall = degToRad 15
    radS0 = degToRad 30
    l a = 2 * pi * ar / (ar + 2) * a
    d c = 0.027 + c * c / pi / ar / 0.8
    clMax = l radStall
    cdS = d clMax
    aa = abs aoa
    g = if aoa < 0 then -1 else 1
    cl = if aa < radStall then l aoa
         else if aa < radS0 then g * rescale radStall radS0 clMax 0 aa
         else 0
    cd = if aa < radStall then d cl
         else rescale radStall (degToRad 180) cdS (cdS * 2) aa
