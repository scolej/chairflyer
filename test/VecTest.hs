module VecTest (tests) where

import Happy
import Handy
import Vec

tests :: [Test]
tests =
  concat [ radTwixtv2Tests
         , radTwixtv3Tests
         ]

radTwixtv2Tests :: [Test]
radTwixtv2Tests =
  let atwi e a b = assertDouble 1e-5 e (radToDeg $ radTwixtv2 a b)
  in prefixTests "radTwixtv2"
     [ "orthogonal anti-clockwise" @@@ atwi (-90) (Vec2 0 1) (Vec2 1 0)
     , "orthogonal clockwise" @@@ atwi 90 (Vec2 1 0) (Vec2 0 1)
     , "opposites" @@@ atwi 180 (Vec2 (-1) (-1)) (Vec2 1 1)
     , "arbitrary acute angle 1" @@@ atwi 45 (Vec2 1 1) (Vec2 0 1)
     , "arbitrary acute angle 2" @@@ atwi 45 (Vec2 1 (-1)) (Vec2 1 0)
     , "arbitrary obtuse angle 1" @@@ atwi 135 (Vec2 1 (-1)) (Vec2 0 1)
     , "arbitrary obtuse angle 2" @@@ atwi 135 (Vec2 (-1) (-1)) (Vec2 1 0)
     , "zero 1" @@@ atwi 0 (Vec2 1 0) (Vec2 1 0)
     , "zero 2" @@@ atwi 0 (Vec2 0 0) (Vec2 1 0)
     , "zero 3" @@@ atwi 0 (Vec2 1 0) (Vec2 0 0)
     ]

radTwixtv3Tests :: [Test]
radTwixtv3Tests =
  let atwi e n a b = assertDouble 1e-5 e (radToDeg $ radTwixtv3 n a b)
  in prefixTests "radTwixtv3"
     [ "zero (1)" @@@ atwi 0  (Vec3 0 0 0) (Vec3 0 0 0) (Vec3 0 0 0)
     , "zero (2)" @@@ atwi 0 (Vec3 1 0 0) (Vec3 1 0 0) (Vec3 1 0 0)
     , "right angle (1)" @@@ atwi 90 (Vec3 0 0 (-1)) (Vec3 1 0 0) (Vec3 0 1 0)
     , "right angle (2)" @@@ atwi (-90) (Vec3 0 0 1) (Vec3 1 0 0) (Vec3 0 1 0)
     , "45 (1)" @@@ atwi 45 (Vec3 0 0 (-1)) (Vec3 1 0 0) (Vec3 1 1 0)
     , "45 (2)" @@@ atwi (-45) (Vec3 0 0 1) (Vec3 1 0 0) (Vec3 1 1 0)
     , "opposites (1)" @@@ atwi 180 (Vec3 0 0 (-1)) (Vec3 1 0 0) (Vec3 (-1) 0 0)
     , "opposites (2)" @@@ atwi 180 (Vec3 0 0 1) (Vec3 1 0 0) (Vec3 (-1) 0 0)
     ]
