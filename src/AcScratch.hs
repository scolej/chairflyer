import AcSystem
import RK4
import Vec
import Text.Printf

rk4 :: Double -> AcState -> AcState
rk4 = rk4step acAddRate acRateScale acRateAdd (computeAcRate hackyJab)

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
hist = take 7000 $ iterate (acClip . rk4 0.3) s0

main :: IO ()
main = writeFile "output.dat" $ unlines $ map (unwords . f) hist
  where
    f s =
      let Vec3 x y z = acPos s
          Vec2 vx vz = acVel s
          t = acTime s
      in map sci [t, x, y, z, vx, vz]

sci :: Double -> String
sci = printf "%15.5e"
