import Handy
import Output

f :: Double -> Double
f = piecewiseLerp
     [ (-8, 1)
     , (-4, -1)
     , (0, 0)
     , (5, 0.5)
     ]

vs :: [[Double]]
vs = map (\x -> [x, f x]) [-10,-9.75..10]

main :: IO ()
main =
  writeData "out.dat" (map (map sci) vs)
