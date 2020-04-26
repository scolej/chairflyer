import LiftDrag
import Output

main :: IO ()
main = do
  let f a = [a, l, d] where (l, d) = liftDrag 7.4 a
  writeData "out.dat" $ map (map sci . f) [-pi, -pi + 0.01 .. pi]
