import NVector
import Vec
import Happy

main :: IO ()
main = do
  runTests $ backAndForth
          ++ destinations

backAndForth :: [Test]
backAndForth =
  let ll  = (-30, 134)
      llr = (pi / 4, pi / 2)
  in [ Test "change between lat long and nvector"
            (assertLL llr (nvecToLL . llToNVec $ llr))
     , Test "change between degrees lat long and nvector"
            (assertLL ll (nvecToLLDeg . llDegToNVec $ ll))
     ]

destinations :: [Test]
destinations =
  let p1   = llDegToNVec (53.320556, -1.729722)
      p2   = llDegToNVec (45, 0)
      p3   = llDegToNVec (45, 90)
      h = degToRad 96.0217
      dist = 124800
      earthCirc = 2 * pi * earthRadius
  in [ Test "south to the equator (1)"
         (assertLL
            (0, 0)
            (nvecToLLDeg $ destination p2 (degToRad 180) (earthCirc / 8)))
     , Test "south to the equator (2)"
         (assertLL
            (0, 90)
            (nvecToLLDeg $ destination p3 (degToRad 180) (earthCirc / 8)))
     , Test "nominal destination"
         (assertLL
            (53.18826954833333, 0.13327449055555557)
            (nvecToLLDeg $ destination p1 h dist))
     ]
