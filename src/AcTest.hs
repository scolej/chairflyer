import AcSystem
import Integrators
import Output
import Vec

rk4 :: Double -> AcState -> AcState
rk4 = rk4step (acRate hackyJab) acStep

s0 :: AcState
s0 =
  AcState { acTime = 0
          , acPos = zerov3
          , acVel = Vec2 0 0
          , acMass = acpMass hackyJab
          , acHeading = 0
          , acPitch = degToRad 5
          }

hist :: [AcState]
hist = take 7000 $ iterate (acClip . rk4 0.1) s0

showState :: AcState -> [String]
showState s =
  let Vec3 x y z = acPos s
      Vec2 vx vz = acVel s
      t = acTime s
  in map sci [t, x, y, z, vx, vz]

main :: IO ()
main =
  writeData "tmp/outputAc.dat" (map showState hist)
