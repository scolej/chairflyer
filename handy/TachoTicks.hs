import Text.Printf

rpmToDeg :: Double -> Double
rpmToDeg rpm = rpm / 4000 * 360 * 0.8 + 0.1 * 360

-- Generate SVG strings for airspeed indicator ticks and labels.
main :: IO ()
main = do
  mapM_ putStrLn $
       map polyLineTick (map (\i -> rpmToDeg $ i * 20 * 100) [1,2..5])
    ++ map polyLineTickMinor (map (\i -> rpmToDeg $ i * 2 * 100) [1,2..20])
    ++ map number (map (\i -> (i, rpmToDeg $ i * 100)) [0, 10, 20, 30, 40])
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
            , "' class='tachoText'>"
            , printf "%.0f" num
            , "</text>"
            ]
