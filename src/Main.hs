import AcSystem
import RK4
import Vec

step :: Double -> AcState -> AcState
step = rk4step acAddRate acRateScale acRateAdd (computeAcRate hackyJab)

initState :: AcState
initState =
  AcState { acTime = 0
          , acPos = zerov3
          , acVel = Vec2 0 0
          , acMass = acpMass hackyJab
          , acHeading = 0
          , acPitch = degToRad 5
          }

printlog :: AcState -> IO ()
printlog = appendFile "ac.log" l
  where Vec3 x y z = acPos s
        Vec2 vx vz = acVel s
        t = acTime s
        l = (unwords $ map sci [t, x, y, z, vx, vz])

simulation :: Double -> AcState -> IO ()
simulation dt s = do
  let s' = step s d
  printlog s'
  threadDelay $ dt * 1e6
  simulation dt s'

main :: IO ()
main = do
  forkIO (simulation 0.3 initState)
  return ()