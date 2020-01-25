import AcSystem
import RK4
import Vec
import Text.Printf

rk4 :: Double -> AcState -> AcState
rk4 = rk4step acAddRate sRateScale sRateAdd computeSpringRate

s0 :: AcState
s0 =
  AcState { acTime = 0
          , acPos = zerov3
          , acVel = zerov3
          , acMass = acpMass hackyJab
          , acHeading = 0
          , acPitch = 5
          }

hist :: [AcState]
hist = take 50 $ iterate (rk4spring 1.2) s0

main :: IO ()
main = writeFile "output.dat" $ unlines $ map (unwords . f) hist
  where
    f s =
      let x = sPos s
          v = sVel s
          t = sTime s
      in map sci [t, x, v]

sci :: Double -> String
sci = printf "%15.5e"
