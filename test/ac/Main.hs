import AcState
import Atmosphere
import Handy
import Jabiru
import Output
import Units
import Vec
import System.Process

s0 :: AcState
s0 =
  AcState
  { acTime = 0
  , acThrottle = 1
  , acAltitude = 0
  , acPos = zerov3
  , acHeadingV = zerov3
  , acHeading = 0
  , acVel = Vec2 0 0
  , acMass = 540
  , acPitch = degToRad 8
  }

takeMinutes :: Double -> [AcState] -> [AcState]
takeMinutes mins = takeWhile (\s -> acTime s < mins * 60)

simple :: [AcState]
simple =
  takeMinutes 2 $ iterate (jabiru (isaWind (const zerov3)) 0.3) s0

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
  writeData "simple.dat" (map showState simple)
  callCommand "gnuplot -e \"figFile='simple.png'\" -e \"datFile='simple.dat'\" plot.plt"
