import RK4
import SpringSystem
import Text.Printf

rk4spring :: Double -> SpringState -> SpringState
rk4spring = rk4step sAddRate sRateScale sRateAdd computeSpringRate

s0 :: SpringState
s0 =
  SpringState { sPos = 1
              , sVel = 0
              , sTime = 0
              }

hist :: [SpringState]
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
