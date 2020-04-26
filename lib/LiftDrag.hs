module LiftDrag where

import Handy

liftDrag :: Double -> Double -> (Double, Double)
liftDrag ar aoa = (cl, cd)
  where
    radStall = degToRad 15
    radS0 = degToRad 20
    l a = 2 * pi * ar / (ar + 2) * a
    d c = 0.027 + c * c / pi / ar / 0.8
    clMax = l radStall
    cdS = d clMax
    cl = if abs aoa < radStall then l aoa
         else if abs aoa < radS0 then rescale radStall radS0 clMax 0 aoa
         else 0
    cd = if abs aoa < radStall then d cl
         else rescale radStall (degToRad 180) cdS (cdS * 2) aoa
