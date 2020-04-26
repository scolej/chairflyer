module Vec where

import Data.List
import Test.HUnit
import Handy

data Vec2 =
  Vec2 Double Double
  deriving (Eq, Show)

data Vec3 =
  Vec3 Double Double Double
  deriving (Eq, Show)

zerov2 :: Vec2
zerov2 = Vec2 0 0

zerov3 :: Vec3
zerov3 = Vec3 0 0 0

zipv3 :: (Double -> Double -> Double) -> Vec3 -> Vec3 -> Vec3
zipv3 f (Vec3 ax ay az) (Vec3 bx by bz) = Vec3 (f ax bx) (f ay by) (f az bz)

zipv2 :: (Double -> Double -> Double) -> Vec2 -> Vec2 -> Vec2
zipv2 f (Vec2 ax ay) (Vec2 bx by) = Vec2 (f ax bx) (f ay by)

addv3 :: Vec3 -> Vec3 -> Vec3
addv3 = zipv3 (+)

sumv3 :: [Vec3] -> Vec3
sumv3 = foldl1 addv3

addv2 :: Vec2 -> Vec2 -> Vec2
addv2 = zipv2 (+)

sumv2 :: [Vec2] -> Vec2
sumv2 = foldl1 addv2

subv3 :: Vec3 -> Vec3 -> Vec3
subv3 = zipv3 (-)

subv2 :: Vec2 -> Vec2 -> Vec2
subv2 = zipv2 (-)

mapv3 :: (Double -> Double) -> Vec3 -> Vec3
mapv3 f (Vec3 ax ay az) = Vec3 (f ax) (f ay) (f az)

mapv2 :: (Double -> Double) -> Vec2 -> Vec2
mapv2 f (Vec2 ax ay) = Vec2 (f ax) (f ay)

scalev3 :: Double -> Vec3 -> Vec3
scalev3 f = mapv3 (* f)

scalev2 :: Double -> Vec2 -> Vec2
scalev2 f = mapv2 (* f)

magv3 :: Vec3 -> Double
magv3 (Vec3 x y z) = sqrt $ x * x + y * y + z * z

magv2 :: Vec2 -> Double
magv2 (Vec2 x y) = sqrt $ x * x + y * y

unitv3 :: Vec3 -> Vec3
unitv3 v =
  if mag == 0
    then zerov3
    else scalev3 (1 / mag) v
  where
    mag = magv3 v

unitv2 :: Vec2 -> Vec2
unitv2 v =
  if mag == 0
    then zerov2
    else scalev2 (1 / mag) v
  where
    mag = magv2 v

downv2 :: Vec2
downv2 = Vec2 0 (-1)

downv3 :: Vec3
downv3 = Vec3 0 0 (-1)

reversev2 :: Vec2 -> Vec2
reversev2 v = scalev2 (-1) v

crossv3 :: Vec3 -> Vec3 -> Vec3
crossv3 (Vec3 ax ay az) (Vec3 bx by bz) =
  Vec3 (ay * bz - az * by)
       (az * bx - ax * bz)
       (ax * by - ay * bx)

crossv2 :: Vec2 -> Vec2 -> Vec3
crossv2 (Vec2 ax ay) (Vec2 bx by) = Vec3 0 0 (ax * by - ay * bx)

dotv2 :: Vec2 -> Vec2 -> Double
dotv2 (Vec2 ax ay) (Vec2 bx by) = ax * bx + ay * by

sign :: Double -> Double
sign x
  | x > 0 = 1
  | x < 0 = -1
  | otherwise = 0

-- Find the signed angle (radians) between two vectors.
-- Positive: anti-clockwise from a to b
-- Negative: clockwise from a to b
radTwixtv2 :: Vec2 -> Vec2 -> Double
radTwixtv2 a b
  | ma == 0 || mb == 0 = 0
  | otherwise = s * acos d
  where
    Vec3 _ _ cz = crossv2 a b
    s
      | cz < 0 = -1
      | otherwise = 1
    d = (dotv2 a b) / (ma * mb)
    ma = magv2 a
    mb = magv2 b

-- FIXME convert these!!

assertFloating :: Double -> Double -> Assertion
assertFloating expected actual =
  if abs (expected - actual) > 1e-7
    then assertFailure $
         intercalate "\n" ["expected: " ++ show expected, "     got: " ++ show actual]
    else return ()

radTwixtv2Tests :: Test
radTwixtv2Tests =
  TestList
    [ "orthogonal anti-clockwise" ~:
      assertFloating (degToRad (-90)) (radTwixtv2 (Vec2 0 1) (Vec2 1 0))
    , "orthogonal clockwise" ~:
      assertFloating (degToRad 90) (radTwixtv2 (Vec2 1 0) (Vec2 0 1))
    , "opposites" ~:
      assertFloating (degToRad 180) (radTwixtv2 (Vec2 (-1) (-1)) (Vec2 1 1))
    , "arbitrary acute angle 1" ~:
      assertFloating (degToRad 45) (radTwixtv2 (Vec2 1 1) (Vec2 0 1))
    , "arbitrary acute angle 2" ~:
      assertFloating (degToRad 45) (radTwixtv2 (Vec2 1 (-1)) (Vec2 1 0))
    , "arbitrary obtuse angle 1" ~:
      assertFloating (degToRad 135) (radTwixtv2 (Vec2 1 (-1)) (Vec2 0 1))
    , "arbitrary obtuse angle 2" ~:
      assertFloating (degToRad 135) (radTwixtv2 (Vec2 (-1) (-1)) (Vec2 1 0))
    , "zero 1" ~: assertFloating 0 (radTwixtv2 (Vec2 1 0) (Vec2 1 0))
    , "zero 2" ~: assertFloating 0 (radTwixtv2 (Vec2 0 0) (Vec2 1 0))
    , "zero 3" ~: assertFloating 0 (radTwixtv2 (Vec2 1 0) (Vec2 0 0))
    ]

--
--
--
