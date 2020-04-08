import Integrators
import SpringSystem
import Output

rk4spring :: Double -> SpringState -> SpringState
rk4spring = rk4step springRate springStep

rk4euler :: Double -> SpringState -> SpringState
rk4euler = eulerStep springRate springStep

s0 :: SpringState
s0 =
  SpringState { sPos = 1
              , sVel = 0
              , sTime = 0
              }

histrk4 :: [SpringState]
histrk4 = takeWhile (\s -> sTime s < 60) $ iterate (rk4spring 5) s0

histEuler :: [SpringState]
histEuler = takeWhile (\s -> sTime s < 60) $ iterate (rk4euler 0.01) s0

showState :: SpringState -> [String]
showState s = map sci [t, x, v]
  where x = sPos s
        v = sVel s
        t = sTime s

main :: IO ()
main = do
  writeData "tmp/outputRK4.dat" (map showState histrk4)
  writeData "tmp/outputEuler.dat" (map showState histEuler)
  putStrLn "Done!"
