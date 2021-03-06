{-# LANGUAGE DeriveGeneric #-}

import AcState
import AcSystem
import Atmosphere
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Controller
import Data.Aeson
import Data.Text
import GHC.Generics
import Handy
import Integrators
import Units
import Vec
import NVector
import qualified Network.WebSockets as WS

--
-- Simulation
--

threadDelaySec :: Double -> IO ()
threadDelaySec s = threadDelay . round $ s * 1000000

-- | Individual simulation time steps, seconds.
simTimeStep :: Double
simTimeStep = 0.4

simStep :: AcSystem -> AcSystem
simStep = stepAcSystem simTimeStep

startState :: AcSystem
startState =
    let ac0 =
          AcState { acTime = 0
                  , acAltitude = 0
                  , acTrack = (llDegToNVec (-37.698329, 145.365335), degToRad 8, 0)
                  , acVel = Vec2 0 0
                  , acMass = acpMass hackyJab
                  , acHeading = 0
                  , acPitch = degToRad 10
                  -- Massive FIXME - if I make this 0 everything goes haywire
                  -- div by 0 somewhere?
                  , acThrottle = 0.1
                  }
    in AcSystem { sysState = ac0
                , sysController = idController
                }

simpleSim :: TVar AcSystem -> IO ()
simpleSim var = forever go
  where f s = iterate simStep s !! 1
        go = do
          atomically $ modifyTVar var f
          threadDelaySec simTimeStep

--
-- Server stuff
--

data Response =
  Response { rAltitude :: Double
           , rAirspeed :: Double
           , rLatLon :: LatLon
           , rHeadingRad :: Double
           , rRpm :: Double
           } deriving (Generic, Show)

instance ToJSON Response where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Response

-- FIXME These heading changes are wrong. Need to use final heading to
-- get the adjusted heading at end of track.

newHeading :: Double -> AcState -> AcState
newHeading deg s0 = s0 { acTrack = (p, h, 0) }
  where (p0, h0, d) = acTrack s0
        p = destination p0 h0 d
        h = degToRad deg

turn :: Double -> AcState -> AcState
turn deg s0 = s0 { acTrack = (p, h, 0) }
  where (p0, h0, d) = acTrack s0
        p = destination p0 h0 d
        h = degToRad deg + h0

adjustPitch :: Double -> AcState -> AcState
adjustPitch deg s0 = s0 { acPitch = p }
  where p0 = acPitch s0
        p = degToRad deg + p0

adjustThrottle :: Double -> AcState -> AcState
adjustThrottle delta s0 = s0 { acThrottle = t }
  where t0 = acThrottle s0
        t = max 0 $ min 1 $ t0 + delta / 100

parseMessage :: String -> AcSystem -> AcSystem
parseMessage ('t':'h':'+':[]) = updateState (adjustThrottle 2)
parseMessage ('t':'h':'-':[]) = updateState (adjustThrottle (-2))
parseMessage ('p':'u':rest) = updateState $ adjustPitch $ read rest
parseMessage ('p':'d':rest) = updateState $ adjustPitch $ (-1) * read rest
parseMessage ('p':rest) = updateState (\a -> a { acPitch = degToRad $ read rest })
parseMessage ('t':rest) = updateState (\a -> a { acThrottle = read rest / 100 })
parseMessage ('h':rest) = updateState $ newHeading $ read rest
parseMessage ('r':rest) = updateState $ turn $ read rest
parseMessage ('l':rest) = updateState (turn $ (-1) * read rest)
parseMessage _ = id

port :: Int
port = 8000

main :: IO ()
main = do
  var <- newTVarIO startState
  forkIO $ simpleSim var
  putStrLn $ "Starting on port " ++ show port
  WS.runServer "127.0.0.1" port (app var)

app :: TVar AcSystem -> WS.ServerApp
app var pending = do
  putStrLn "Got pending connection."
  conn <- WS.acceptRequest pending
  let send = do
        s <- atomically $ readTVar var
        let ac = sysState s
            z = acAltitude ac
            v = acVel ac
            (p0, h, d) = acTrack ac
            ll = nvecToLLDeg $ destination p0 h d
            resp = Response { rAltitude = mToFt z
                            , rAirspeed = mpsToKnots (magv2 v)
                            , rLatLon = ll
                            , rHeadingRad = h
                            , rRpm = acThrottle ac * acpMaxPropRpm hackyJab
                            }
        WS.sendTextData conn $ encode resp
        threadDelaySec simTimeStep
  forkIO $ forever send
  let receive = do
        txt <- unpack <$> WS.receiveData conn
        putStrLn $ "Received " ++ txt
        let u = parseMessage $ txt
        atomically $ modifyTVar var u
  forever receive
  -- FIXME if we use 'forever' here, what happens when the connection closes?
