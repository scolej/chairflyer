module NVectorTest (tests) where

import Happy
import Vec
import Handy
import NVector

tests :: [Test]
tests =
  concat [ backAndForth
         , destinations
         , destinationVs
         , headingTests
         , headingIsos
         , angleWrapping
         , initialHeadingTests
         ]

backAndForth :: [Test]
backAndForth =
  let ll  = (-30, 134)
      llr = (pi / 4, pi / 2)
  in [ "change between lat long and nvector" @@@
            assertLL llr (nvecToLL . llToNVec $ llr)
     , "change between degrees lat long and nvector" @@@
            assertLL ll (nvecToLLDeg . llDegToNVec $ ll)
     ]

destinations :: [Test]
destinations =
  let p1        = llDegToNVec (53.320556, -1.729722)
      p2        = llDegToNVec (45, 0)
      p3        = llDegToNVec (45, 90)
      h         = degToRad 96.0217
      dist      = 124800
      earthCirc = 2 * pi * earthRadius
  in [ "south to the equator (1)" @@@
         assertLL
            (0, 0)
            (nvecToLLDeg $ destination p2 (degToRad 180) (earthCirc / 8))
     , "south to the equator (2)" @@@
         assertLL
            (0, 90)
            (nvecToLLDeg $ destination p3 (degToRad 180) (earthCirc / 8))
     , "over the pole" @@@
         assertLL
            (45, 180)
            (nvecToLLDeg $ destination p2 (degToRad 0) (earthCirc / 4))
     -- TODO copy paste results, need some good cases
     , "nominal destination" @@@
         assertLL
            (53.1884335712, 0.131199053)
            (nvecToLLDeg $ destination p1 h dist)
     ]

destinationVs :: [Test]
destinationVs =
  let tol = 1e-6
      dist = 1000
      t = dist / earthRadius
      test h d p e =
        let a = destinationV h d p
        in assertV3 tol e a
  in [ "north from equator" @@@
       let e = Vec3 (sin t) (cos t) 0
       in test (Vec3 1 0 0) dist (Vec3 0 1 0) e
     , "south from equator" @@@
       let e = Vec3 (-(sin t)) (cos t) 0
       in test (Vec3 (-1) 0 0) dist (Vec3 0 1 0) e
     , "east along equator" @@@
       let e = Vec3 0 (cos t) (sin t)
       in test (Vec3 0 0 1) dist (Vec3 0 1 0) e
     , "west along equator" @@@
       let e = Vec3 0 (cos t) (-(sin t))
       in test (Vec3 0 0 (-1)) dist (Vec3 0 1 0) e
     ]

headingTests :: [Test]
headingTests =
  let tol = 1e-3
      test e p h = assertDouble tol e (radToDeg $ heading p h)
  in [ "north" @@@ test   0 (Vec3 0 1 0) (Vec3   1   0   0 )
     , "south" @@@ test 180 (Vec3 0 1 0) (Vec3 (-1)  0   0 )
     , "east"  @@@ test  90 (Vec3 0 1 0) (Vec3   0   0   1 )
     , "west"  @@@ test 270 (Vec3 0 1 0) (Vec3   0   0 (-1))
     --
     , "north east" @@@ test  45 (Vec3 0 1 0) (unitv3 (Vec3 1 0   1 ))
     , "north west" @@@ test 315 (Vec3 0 1 0) (unitv3 (Vec3 1 0 (-1)))
     --
     , "north 2" @@@ test   0 (Vec3 0 (-1) 0) (Vec3   1   0   0 )
     , "south 2" @@@ test 180 (Vec3 0 (-1) 0) (Vec3 (-1)  0   0 )
     , "east 2"  @@@ test  90 (Vec3 0 (-1) 0) (Vec3   0   0 (-1))
     , "west 2"  @@@ test 270 (Vec3 0 (-1) 0) (Vec3   0   0   1 )
     ]

headingIsos :: [Test]
headingIsos =
  let p = llDegToNVec (-37.698329, 145.365335)
      test deg =
        assertDouble 1e-6 deg (radToDeg . heading p . headingVector p . degToRad $ deg)
  in [ "heading iso 1" @@@ test 0
     , "heading iso 2" @@@ test 90
     , "heading iso 3" @@@ test 180
     , "heading iso 4" @@@ test 270
     ]

angleWrapping :: [Test]
angleWrapping =
  let asd e a = assertDouble 1e-6 e (radToDeg . wrapHeading . degToRad $ a)
  in [ "negative wrap" @@@ asd 180 (-180)
     , "positive wrap" @@@ asd 10 370
     , "unchanged (1)" @@@ asd 10 10
     , "unchanged (2)" @@@ asd 190 190
     ]

initialHeadingTests :: [Test]
initialHeadingTests =
  let ini a b e =
        assertDouble 1e-3 e $
        radToDeg (initialHeading (llDegToNVec a) (llDegToNVec b))
  in prefixTests "initial heading"
     [ "to the north pole" @@@ ini (0, 0) (90, 0) 0
     , "to the south pole" @@@ ini (0, 0) (-90, 0) 180
     , "to the east" @@@ ini (0, 0) (0, 1) 90
     , "to the west" @@@ ini (0, 0) (0, (-1)) 270
     , "Lilydale 36" @@@ ini (-37.698329, 145.365335) (-37.686684, 145.367438) 8.133
     , "Lilydale 18" @@@ ini (-37.686684, 145.367438) (-37.698329, 145.365335) 188.132
     ]
