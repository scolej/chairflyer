import AcSystem
import Integrators
import Output
import Vec
import Atmosphere

rk4 :: Double -> AcState -> AcState
rk4 = rk4step (acRate isa hackyJab) acStep

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
hist = takeWhile (\s -> acTime s < 30 * 60) $ iterate (acClip . rk4 0.1) s0

showState :: AcState -> [String]
showState s =
  let Vec3 x y z = acPos s
      Vec2 vx vz = acVel s
      t = acTime s
  in map sci [t, x, y, z, vx, vz]

main :: IO ()
main =
  writeData "out.dat" (map showState hist)
