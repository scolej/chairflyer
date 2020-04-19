import Text.Printf

-- Generate SVG strings for airspeed indicator ticks and labels.
main :: IO ()
main = do
  mapM_ putStrLn $
       map polyLineTick (map knotsToTheta [20,30..160])
    ++ map polyLineTickMinor (map knotsToTheta [25,35..155])
    ++ map number (map (\n -> (n, knotsToTheta n)) [20,40..160])
  return ()

polyLineTickMinor :: Double -> String
polyLineTickMinor theta =
  "<polyline transform='rotate("
  ++ printf "%.1f" theta
  ++ ")' points='0,27 0,29' stroke-width='0.8' stroke='white'/>"

polyLineTick :: Double -> String
polyLineTick theta =
  "<polyline transform='rotate("
  ++ printf "%.1f" theta
  ++ ")' points='0,25 0,29' stroke-width='1' stroke='white'/>"

degToRad :: Double -> Double
degToRad = (*) (pi / 180)

number :: (Double, Double) -> String
number (num, theta) =
  let r = 20
      x = r * sin (degToRad $ -theta)
      y = r * cos (degToRad $ -theta)
  in concat [ "<text dominant-baseline='middle' text-anchor='middle' x='"
            , printf "%.1f" x
            , "' y='"
            , printf "%.1f" y
            , "' class='asText'>"
            , printf "%.0f" num
            , "</text>"
            ]

knotsToTheta :: Double -> Double
knotsToTheta knots =
  (knots / 165) ** 2 * 360 + 180
