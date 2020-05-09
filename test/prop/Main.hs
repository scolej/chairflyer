import Output
import Prop

kcs :: [[Double]]
kcs = map f [0,5..100]
  where f v = let j = v / (rpmToRps 3100) / 1.5
              in [j, thrustCoeff j]

vs :: [[Double]]
vs = map (\v -> [v, propThrust 1.5 1.225 (rpmToRps 2800) v]) [0,5..100]

ds :: [[Double]]
ds = map (\d -> [d, propThrust 1.5 d (rpmToRps 2800) 30]) [1.3,1.2..0]

main :: IO ()
main = do
  writeData "vel.dat" (map (map sci) vs)
  writeData "rhos.dat" (map (map sci) ds)
  writeData "kcs.dat" (map (map sci) kcs)
