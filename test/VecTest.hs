module VecTest (tests) where

import Happy
import Handy
import Vec

tests :: [Test]
tests =
  concat [ radTwixtv2Tests
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
