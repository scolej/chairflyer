import Output
import AcState

vs :: [[Double]]
vs = map (\v -> [v, hackyThrustAvailable 1000 1.225 v]) [0,5..100]

ds :: [[Double]]
ds = map (\d -> [d, hackyThrustAvailable 1000 d 30]) [1.3,1.2..0]

main :: IO ()
main = do
  writeData "vel.dat" (map (map sci) vs)
  writeData "rhos.dat" (map (map sci) ds)