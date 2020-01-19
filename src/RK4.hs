module RK4 where

import Vec

rk4stepv2 ::
     [Vec2]
  -> (Double -> [Vec2] -> [Vec2])
  -> Double
  -> [Vec2]
rk4stepv2 ys f h =
  foldl (zipWith addv2) (repeat zerov2) $
     [ys] ++ (map (map (scalev2 (h / 6))) [f1, map (scalev2 2) f2, map (scalev2 2) f3, f4])
  where
    f1, f2, f3, f4 :: [Vec2]
    f1 = f 0 ys
    f2 = f (h / 2) (zipWith addv2 ys (map (scalev2 (h / 2)) f1))
    f3 = f (h / 2) (zipWith addv2 ys (map (scalev2 (h / 2)) f2))
    f4 = f h (zipWith addv2 ys (map (scalev2 h) f3))

rk4StepBody2 ::
     Double
  -> Body
  -> Body
rk4StepBody2 t b =
  let [p', v'] = rk4stepv2 [bodyPos b, bodyVel b] (bodyAcc b) t
  in b { bodyPos = p'
       , bodyVel = v'
       }

type Pos2 = Vec2

type Vel2 = Vec2

type Acc2 = Vec2

data Body =
  Body
    { bodyPos :: Vec2
    , bodyVel :: Vec2
    , bodyMass :: Double
    , bodyAcc :: Double -> [Vec2] -> [Vec2]
    }
