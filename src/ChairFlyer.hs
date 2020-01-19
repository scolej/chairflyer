module ChairFlyer where

import Data.List
import Debug.Trace
import Test.HUnit
import Text.Printf

-- | Aircraft properties.
data AcProps =
  AcProps
    { acpMass :: Double
    , acpLiftingArea :: Double
    , acpDraggingArea :: Double
    , acpMaxThrust :: Double
    }
  deriving (Show)

hackyJab :: AcProps
hackyJab =
  AcProps
    { acpMass = 540
    , acpLiftingArea = 10
    , acpDraggingArea = 2
    , acpMaxThrust = 1000
    }

data PilotInput =
  PilotInput
    { inputThrottleFraction :: Double
    , inputPitchRad :: Double
    , inputHeadingRad :: Double
    }
  deriving (Show)

someInput =
  PilotInput
    { inputThrottleFraction = 1.0
    , inputPitchRad = degToRad 6
    , inputHeadingRad = 0
    }

rk4step ::
     [Double] -- ^ starting values
  -> (Double -> [Double] -> [Double])
  -> Double -- ^ time step
  -> Double
rk4step y0 f h = y0 + h / 6 * (f1 + 2 * f2 + 2 * f3 + f4)
  where
    f1 = f 0 y0
    f2 = f (h / 2) (y0 + h / 2 * f1)
    f3 = f (h / 2) (y0 + h / 2 * f2)
    f4 = f h (y0 + h * f3)

data AcState =
  AcState
    { acPos :: Vec3
    , acVel :: Vec2
    , acMass :: Double
    , acPitch :: Double
    , acHeading :: Double
    }
  deriving (Show)

someState =
  AcState
    {acPos = zerov3, acVel = zerov2, acMass = 540, acPitch = 0, acHeading = 0}

data IState =
  IState
    { isLift :: Vec2
    , isDrag :: Vec2
    , isThrust :: Vec2
    , isWeight :: Vec2
    , isAoa :: Double
    , isCl :: Double
    , isCd :: Double
    , isQ :: Double
    , isHeadingRate :: Double
    , isPitchRate :: Double
    }

computeIState :: PilotInput -> AcProps -> AcState -> IState
computeIState input acp ac =
  IState
    { isLift = vup `scalev2` (q * (acpLiftingArea acp) * cl)
    , isDrag = vforward `scalev2` (-q * (acpDraggingArea acp) * cd)
    , isThrust =
        aforward `scalev2` (inputThrottleFraction input * acpMaxThrust acp)
    , isWeight = downv2 `scalev2` (acMass ac * 9.81)
    , isAoa = aoa
    , isCl = cl
    , isCd = cd
    , isQ = q
    , isHeadingRate = (inputHeadingRad input - acHeading ac) / 3
    , isPitchRate = (inputPitchRad input - acPitch ac) / 3
    }
  where
    p = acPitch ac
    vel@(Vec2 vx vz) = acVel ac
    magv = magv2 vel
    aforward = Vec2 (cos p) (sin p)
    vforward = unitv2 vel
    vup = unitv2 $ Vec2 (-vz) vx
    aoa = radTwixtv2 vforward aforward
    density = 1.23
    q = 0.5 * density * magv * magv
    ar = 7.4
    cl =
      if abs aoa > degToRad 15
        then error ":("
        else 2 * pi * ar / (ar + 2) * aoa
    cd = 0.027 + cl * cl / pi / ar / 0.8

clipGround :: AcState -> AcState
clipGround ac = ac {acPos = pos', acVel = vel'}
  where
    onGround = pz <= 0
    Vec3 px py pz = acPos ac
    pos' =
      Vec3
        px
        py
        (if onGround
           then 0
           else pz)
    Vec2 vx vz = acVel ac
    vel' =
      Vec2
        vx
        (if (onGround && vz < 0)
           then 0
           else vz)

step :: IState -> Double -> AcState -> AcState
step is t ac =
  clipGround
    ac
      { acPos = pos `addv3` (vel3 `scalev3` t)
      , acVel = vel'
      , acPitch = p + t * isPitchRate is
      , acHeading = h + t * isHeadingRate is
      }
  where
    pos = acPos ac
    h = acHeading ac
    p = acPitch ac
    acc =
      (foldl addv2 zerov2 [isLift is, isWeight is, isThrust is, isDrag is]) `scalev2`
      (1 / acMass ac)
    vel'@(Vec2 vx' vz') = (acVel ac) `addv2` (acc `scalev2` t)
    vel3 = Vec3 (vx' * sin h) (vx' * cos h) vz'

type AcState2 = (AcState, IState)

step' :: AcState -> AcState2
step' ac = (ac, computeIState someInput hackyJab ac)

step2 :: AcState2 -> AcState2
step2 (ac, is) = (ac', is')
  where
    ac' = step is 0.2 ac
    is' = computeIState someInput hackyJab ac'

writeDataFile :: [AcState2] -> IO ()
writeDataFile states = writeFile "output.dat" ls
  where
    ls = unlines $ map dataLine states
    dataLine (ac, is) =
      let (Vec3 x y z) = acPos ac
          vel@(Vec2 vx vz) = acVel ac
          vmag = magv2 vel
          aoa = radToDeg (isAoa is)
          p = radToDeg (acPitch ac)
          cl = isCl is
          cd = isCd is
       in unwords
            [ sci x
            , sci y
            , sci z
            , sci vx
            , sci vz
            , sci vmag
            , sci aoa
            , sci p
            , sci cl
            , sci cd
            ]

sci :: Double -> String
sci = printf "%15.5e"

sim :: IO ()
sim = do
  writeDataFile $ take 10000 $ iterate step2 (step' someState)
  return ()
