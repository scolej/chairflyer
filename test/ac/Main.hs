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
          , acAltitude = 0
          , acPos = zerov3
          , acHeadingV = zerov3
          , acHeading = 0
          , acVel = Vec2 0 0
          , acMass = acpMass hackyJab
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

speedChanger :: Controller AcState
speedChanger = Controller $
  \dt s0 -> let Controller c =
                    if acTime s0 < 600
                    then airspeedController (knotsToMps 65)
                    else airspeedController (knotsToMps 80)
                -- FIXME Under this arrangement we get lucky because 'airspeedController'
                -- doesn't carry any state. How to compose controllers like this _and_
                -- pipe their own state along?
                (s, _) = c dt s0
            in (s, speedChanger)

histSpeedChanger :: [AcState]
histSpeedChanger =
  map sysState $ takeMinutes 30 $ iterate (stepAcSystem 0.3) sys0
  where sys0 =
          AcSystem { sysState = s0
                   , sysController = speedChanger
                   }

--
--
--

showState :: AcState -> [String]
showState s =
  let z = acAltitude s
      Vec2 vx vz = acVel s
      t = acTime s
      p = radToDeg $ acPitch s
      a = radToDeg $ alpha s
  in map sci [t, mToFt z, mpsToKnots vx, mpsToFpm vz, p, a]

main :: IO ()
main = do
  writeData "speedChanger.dat" (map showState histSpeedChanger)
  writeData "cutThrottle.dat" (map showState histClimbCutThrottle)
