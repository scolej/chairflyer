import Atmosphere
import Output

layer :: Double -> [Double]
layer h = [h, isaPressure h, isaTemperature h, isaDensity h]

layers :: [[Double]]
layers = map layer [0,100..5000]

main :: IO ()
main =
  writeData "tmp/atmosphere.dat" (map (map sci) layers)