import RK4
import Vec
import Text.Printf

spring :: Double -> [Vec2] -> [Vec2]
spring _ xs =
  let Vec2 _ y = xs !! 0
      v = xs !! 1
      d = 0.1 * (0.5 - y)
      a = scalev2 d (Vec2 0 1) `subv2` (scalev2 0.1 v)
  in [v, a]

b0 :: Body
b0 = Body { bodyPos = Vec2 0 0
          , bodyVel = Vec2 0 0
          , bodyMass = 10
          , bodyAcc = spring
          }

hist :: [Body]
hist = take 200 $ iterate (rk4StepBody2 1) b0

main :: IO ()
main = writeFile "output.dat" $ unlines $ map (unwords . f) hist
  where f b = let Vec2 x y = bodyPos b
                  Vec2 vx vy = bodyVel b
              in map sci [x, y, vx, vy]

sci :: Double -> String
sci = printf "%15.5e"
