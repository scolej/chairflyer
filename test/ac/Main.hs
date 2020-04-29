import AcState
import AcSystem
import Atmosphere
import Controller
import Handy
import Integrators
import Output
import Units
import Vec

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

takeMinutes :: Double -> [AcSystem] -> [AcSystem]
takeMinutes mins = takeWhile (\s -> (acTime . sysState) s < mins * 60)

--
-- No controllers.
-- Just climb, cut throttle and descend.
--

histClimbCutThrottle :: [AcState]
histClimbCutThrottle =
  map sysState $ takeMinutes 10 $ iterate (u . stepAcSystem 0.3) sys0
  where sys0 =
          AcSystem { sysState = s0
                   , sysController = idController
                   }
        u :: AcSystem -> AcSystem
        u = updateState (\a -> if acTime a < 300
                               then a { acPitch = degToRad 10
                                      , acThrottle = 1
                                      }
                               else a { acPitch = 0
                                      , acThrottle = 0
                                      })

--
-- Try out the crappy speed controller.
-- Initial speed & then a change.
--

speedChanger :: ControlStep AcState Double
speedChanger s =
  if acTime s < 600
  then airspeedController (knotsToMps 65) s
  else airspeedController (knotsToMps 100) s

sys0 = AcSystem { sysState = s0
                , sysController =
                    Controller { cStep = speedChanger
                               , cState = 0
                               }
                }

histSpeedChanger :: [AcState]
histSpeedChanger =
  map sysState $ takeMinutes 30 $ iterate (stepAcSystem 0.3) sys0
  where sys0 =
          AcSystem { sysState = s0
                   , sysController =
                     Controller { cStep = speedChanger
                                , cState = 0
                                }
                   }

--
--
--

showState :: AcState -> [String]
showState s =
  let Vec3 x y z = acPos s
      Vec2 vx vz = acVel s
      t = acTime s
      p = radToDeg $ acPitch s
      a = radToDeg $ alpha s
  in map sci [t, x, y, mToFt z, mpsToKnots vx, mpsToFpm vz, p, a]

main :: IO ()
main = do
  writeData "speedChanger.dat" (map showState histSpeedChanger)
  writeData "cutThrottle.dat" (map showState histClimbCutThrottle)
