import RK4
import SpringSystem
import Text.Printf

rk4spring :: Double -> SpringState -> SpringState
rk4spring = rk4step sAddRate sRateScale sRateAdd computeSpringRate

s0 :: SpringState
s0 =
  SpringState { sPos = 0
              , sVel = 3
              }

hist :: [SpringState]
hist = take 100 $ iterate (rk4spring 0.1) s0

main :: IO ()
main = writeFile "output.dat" $ unlines $ map (unwords . f) hist
  where
    f s =
      let x = sPos s
          v = sVel s
      in map sci [x, v]

sci :: Double -> String
sci = printf "%15.5e"
