import AcSystem
import Atmosphere
import Control.Concurrent
import Control.Monad
import Data.List
import Integrators
import Output
import System.IO
import Text.Printf
import Units
import Vec

--
-- Pretty printing
--

showDist :: Double -> String
showDist x = printf "%7.0f m" x

showAltitude :: Double -> String
showAltitude x = printf "%7.0f ft" (mToFt x)

showVs :: Double -> String
showVs x = printf "%4.0f fpm" (mpsToFpm x)

showSpeed :: Double -> String
showSpeed x = printf "%4.0f kts" (mpsToKnots x)

showTime :: Double -> String
showTime t = printf "%4.1f s" t

showAngle :: Double -> String
showAngle a = printf "%4.1f°" (radToDeg a)

showHeading :: Double -> String
showHeading h = printf "%03.0f" (radToDeg h)

showCart :: Vec2 -> String
showCart (Vec2 x y) = printf "%5.1f,%5.1f NM" (mToNm x) (mToNm y)

--
-- Output
--

showState :: AcState -> String
showState ac = intercalate " | " [showHeading h, showSpeed v, showAltitude z, showCart p, showVs vs, showAngle aoa]
  where (Vec3 x y z) = acPos ac
        p = Vec2 x y
        vel@(Vec2 _ vs) = acVel ac
        v = magv2 $ vel
        h = acHeading ac
        aoa = alpha ac

showGnuPlot :: AcState -> String
showGnuPlot ac =
  unwords $ map sci [ acTime ac
                    , radToDeg (acHeading ac)
                    , mToNm x
                    , mToNm y
                    , mToFt z
                    , mpsToFpm vz
                    , mpsToKnots (magv2 v)
                    , radToDeg (alpha ac)
                    ]
  where Vec3 x y z = acPos ac
        v@(Vec2 _ vz) = acVel ac

--
--
--

-- | Delay between writes to output pipe, seconds.
outputDelay :: Double
outputDelay = 1

-- | Individual simulation time steps, seconds.
simTimeStep :: Double
simTimeStep = 0.2

simStep :: AcState -> AcState
simStep = acClip . rk4step (acRate isa hackyJab) acStep simTimeStep

startState :: AcState
startState =
    AcState { acTime = 0
            , acPos = zerov3
            , acVel = Vec2 0 0
            , acMass = acpMass hackyJab
            , acHeading = 0
            , acPitch = degToRad 5
            }

data Env =
  Env { pipeOut :: Handle
      , pipeIn :: Handle
      }

parseInput :: [String] -> (AcState -> AcState)
parseInput [] = id
parseInput ("h":arg:[]) = \ac -> ac { acHeading = degToRad $ read arg }
parseInput ("p":arg:[]) = \ac -> ac { acPitch = degToRad $ read arg }
parseInput _ = id

mainLoop :: Env -> AcState -> IO AcState
mainLoop env s0 = do
  ready <- hReady (pipeIn env)
  input <- if ready
           then hGetLine (pipeIn env)
           else return ""

  let ws = words input
  unless (null ws) $ putStrLn $ show ws
  let s1 = (parseInput ws) s0
  let n = 1 + round (outputDelay / simTimeStep)
  let s2 = iterate simStep s1 !! n

  hPutStrLn (pipeOut env) $ (showGnuPlot s1)
  hFlush (pipeOut env)

  threadDelay . round $ outputDelay * 1000000
  mainLoop env s2

main :: IO ()
main = do
  putStrLn "The beginning."
  -- Create pipes
  w <- openFile "output" WriteMode
  r <- openFile "input" ReadMode
  --(_, w) <- createPipe
  --(r, _) <- createPipe
  -- Loop, read input, update state, write output
  let e = Env { pipeOut = w
              , pipeIn = r
              }
  mainLoop e startState
  return ()
