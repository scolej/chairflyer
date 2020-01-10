module ChairFlyer where

import Debug.Trace

data Vec2 = Vec2 Double Double
  deriving (Eq, Show)

data Vec3 = Vec3 Double Double Double
  deriving (Eq, Show)

zerov2 :: Vec2
zerov2 = Vec2 0 0

zerov3 :: Vec3
zerov3 = Vec3 0 0 0

zipv3 :: (Double -> Double -> Double) -> Vec3 -> Vec3 -> Vec3
zipv3 f (Vec3 ax ay az) (Vec3 bx by bz) =
  Vec3 (f ax bx) (f ay by) (f az bz)

zipv2 :: (Double -> Double -> Double) -> Vec2 -> Vec2 -> Vec2
zipv2 f (Vec2 ax ay) (Vec2 bx by) =
  Vec2 (f ax bx) (f ay by)

addv3 :: Vec3 -> Vec3 -> Vec3
addv3 = zipv3 (+)

addv2 :: Vec2 -> Vec2 -> Vec2
addv2 = zipv2 (+)

subv3 :: Vec3 -> Vec3 -> Vec3
subv3 = zipv3 (-)

subv2 :: Vec2 -> Vec2 -> Vec2
subv2 = zipv2 (-)

mapv3 :: (Double -> Double) -> Vec3 -> Vec3
mapv3 f (Vec3 ax ay az) = Vec3 (f ax) (f ay) (f az)

mapv2 :: (Double -> Double) -> Vec2 -> Vec2
mapv2 f (Vec2 ax ay) = Vec2 (f ax) (f ay)

scalev3 :: Vec3 -> Double -> Vec3
scalev3 v f = mapv3 (* f) v

scalev2 :: Vec2 -> Double -> Vec2
scalev2 v f = mapv2 (* f) v

magv3 :: Vec3 -> Double
magv3 (Vec3 x y z) = sqrt $ x * x + y * y + z * z

magv2 :: Vec2 -> Double
magv2 (Vec2 x y) = sqrt $ x * x + y * y

unitv2 :: Vec2 -> Vec2
unitv2 v =
  if mag == 0 then zerov2 else scalev2 v (1 / mag)
  where mag = magv2 v

downv2 :: Vec2
downv2 = Vec2 0 (-1)

downv3 :: Vec3
downv3 = Vec3 0 0 (-1)

reversev2 :: Vec2 -> Vec2
reversev2 v = scalev2 v (-1)

crossv2 :: Vec2 -> Vec2 -> Vec3
crossv2 (Vec2 ax ay) (Vec2 bx by) =
  Vec3 0 0 (ax * by - ay * bx)

sign :: Double -> Double
sign x | x > 0 = 1
       | x < 0 = -1
       | otherwise = 0

-- Find the signed angle (radians) between two vectors.
-- Positive: anti-clockwise from a to b
-- Negative: clockwise from a to b
radTwixtv2 :: Vec2 -> Vec2 -> Double
radTwixtv2 a b =
  if cz == 0
    then 0
    else sign cz * asin ((abs cz) / (magv2 a * magv2 b))
  where Vec3 _ _ cz = crossv2 a b

--
--
--

degToRad :: Double -> Double
degToRad = (*) (pi / 180)

radToDeg :: Double -> Double
radToDeg = (*) (180 / pi)

data PilotInput =
  PilotInput { inputThrottleFraction :: Double
             , inputPitchRad :: Double
             , inputHeadingRad :: Double
             }
  deriving Show

someInput =
  PilotInput { inputThrottleFraction = 1.0
             , inputPitchRad = degToRad 1
             , inputHeadingRad = 0
             }

someState =
  AcState { acPos = zerov3
          , acVel = zerov2
          , acMass = 540
          }

data IState =
  IState { isLift :: Vec2
         , isDrag :: Vec2
         , isThrust :: Vec2
         , isWeight :: Vec2
         , isAoaDeg :: Double
         , isCl :: Double
         , isCd :: Double
         , isQ :: Double
         }

data AcProps =
  AcProps { acpMass :: Double
          , acpLiftingArea :: Double
          , acpDraggingArea :: Double
          , acpMaxThrust :: Double
          }

data AcState =
  AcState { acPos :: Vec3
          , acVel :: Vec2
          , acMass :: Double
          }
  deriving Show

clipGround :: AcState -> AcState
clipGround ac =
  ac { acPos = pos'
     , acVel = vel'
     }
  where onGround = pz <= 0
        Vec3 px py pz = acPos ac
        pos' = Vec3 px py (if onGround then 0 else pz)
        Vec2 vx vz = acVel ac
        vel' = Vec2 vx (if (onGround && vz < 0) then 0 else vz)

step :: PilotInput -> AcState -> Double -> AcState
step input ac secs =
  ac { acPos = pos `addv3` (vel3 `scalev3` secs)
     , acVel = vel'
     }
  where m = acMass ac
        pos = acPos ac
        vel@(Vec2 vx vz) = acVel ac
        h = inputHeadingRad input
        p = inputPitchRad input
        maxThrust = 1000
        aforward = Vec2 (cos p) (sin p)
        vup = Vec2 (-vz) vx
        vforward = unitv2 vel
        thrust = aforward `scalev2` (inputThrottleFraction input * maxThrust)
        density = 1.23
        q = 0.5 * density * v * v
        sl = 10 -- Lifting surface area
        sd = 3 -- Dragging surface area
        aoa = radTwixtv2 vel aforward
        radStall = degToRad 12
        cl = if aoa > radStall || aoa < (-radStall) then 0 else aoa * 1
        cd = if aoa > radStall || aoa < (-radStall) then 1 else (abs aoa) * 0.1
        lift = vup `scalev2` (q * sl * cl)
        weight = downv2 `scalev2` (m * 9.81)
        drag = vforward `scalev2` (q * sd * cd * (-1))
        acc = (foldl addv2 zerov2 [lift, weight, thrust, drag]) `scalev2` (1 / m)
        vel'@(Vec2 vx' vz') = (acVel ac) `addv2` (acc `scalev2` secs)
        v = magv2 vel
        vel3 = Vec3 (vx' * sin h) (vx' * cos h) vz'

writeDataFile :: [AcState] -> IO ()
writeDataFile states = writeFile "output.dat" ls
  where ls = unlines $ header : map dataLine states
        header = "x y z vx vz vmag"
        dataLine ac =
          let (Vec3 x y z) = acPos ac
              vel@(Vec2 vx vz) = acVel ac
              vmag = magv2 vel
          in unwords [show x, show y, show z, show vx, show vz, show vmag]