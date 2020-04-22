import AcState
import AcSystem
import Atmosphere
import Integrators
import Output
import Units
import Vec

pin :: PilotInput
pin = PilotInput { piThrottle = 1
                 , piPitch = degToRad 0
                 }

s0 :: AcState
s0 =
  AcState { acTime = 0
          , acThrottle = 1
          , acPos = zerov3
          , acVel = Vec2 0 0
          , acMass = acpMass hackyJab
          , acHeading = 0
          , acPitch = 0
          }

sys0 = AcSystem { sysState = s0
                , sysInput = pin
                , sysController = Controller { cStep = airspeedController (knotsToMps 80)
                                             , cState = degToRad 0
                                             }
                }

hist :: [AcState]
hist = map sysState $ takeWhile (\s -> (acTime . sysState) s < 30 * 60) $ iterate (stepAcSystem 0.5) sys0

showState :: AcState -> [String]
showState s =
  let Vec3 x y z = acPos s
      Vec2 vx vz = acVel s
      t = acTime s
      p = radToDeg $ acPitch s
  in map sci [t, x, y, z, vx, vz, p]

main :: IO ()
main =
  writeData "out.dat" (map showState hist)
