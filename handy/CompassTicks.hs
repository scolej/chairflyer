import Text.Printf
import Data.Fixed

-- Generate SVG strings for magnetic compass.
main :: IO ()
main = do
  let a = 0 - 150
      b = 360 + 150
  let maj = [a,a+30..b]
      med = filter (\x -> not (elem x maj)) [a,a+10..b]
      min = filter (\x -> not (elem x maj || elem x med)) [a,a+5..b]
  mapM_ putStrLn $
       map (polyLineTickMinor  . fromDeg) min
    ++ map (polyLineTickMedium . fromDeg) med
    ++ map (polyLineTickMajor  . fromDeg) maj
    ++ map number maj
  return ()

fromDeg :: Double -> Double
fromDeg x = (x - 360) * (-1.3)

polyLineTickMinor :: Double -> String
polyLineTickMinor x =
  "<polyline points='"
  ++ unwords [showCoord x (3), showCoord x 9]
  ++ "' stroke-width='1.5' stroke='white'/>"

polyLineTickMedium :: Double -> String
polyLineTickMedium x =
  "<polyline points='"
  ++ unwords [showCoord x (-1), showCoord x 9]
  ++ "' stroke-width='1.5' stroke='white'/>"

polyLineTickMajor :: Double -> String
polyLineTickMajor x =
  "<polyline points='"
  ++ unwords [showCoord x (-5), showCoord x 9]
  ++ "' stroke-width='2.1' stroke='white'/>"

showCoord :: Double -> Double -> String
showCoord x y = printf "%.2f,%.2f" x y

number :: Double -> String
number deg =
  let d :: Double
      d = (deg `mod'` 360) / 10
      t | d == 0 = "N"
        | d == 9 = "E"
        | d == 27 = "W"
        | d == 18 = "S"
        | otherwise = printf "%.0f" d
  in concat [ "<text dominant-baseline='middle' text-anchor='middle' "
            , printf "x='%.1f' y='-12' " (fromDeg deg)
            , "class='compassText'>"
            , t
            , "</text>"
            ]
