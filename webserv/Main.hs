{-# LANGUAGE DeriveGeneric #-}

import AcState
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Aeson
import Data.Text
import Data.Fixed
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

simStep :: AcState -> AcState
simStep = acClip . rk4step jabRate acStep simTimeStep

startState :: AcState
startState =
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

simpleSim :: TVar AcState -> IO ()
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

wrapHeadingRad :: Double -> Double
wrapHeadingRad r = mod' r (2 * pi)

turn :: Double -> AcState -> AcState
turn deg s0 = s0 { acTrack = (p, h, 0) }
  where (p0, h0, d) = acTrack s0
        p = destination p0 h0 d
        h = wrapHeadingRad (degToRad deg + h0)

adjustPitch :: Double -> AcState -> AcState
adjustPitch deg s0 = s0 { acPitch = p }
  where p0 = acPitch s0
        p = degToRad deg + p0

adjustThrottle :: Double -> AcState -> AcState
adjustThrottle delta s0 = s0 { acThrottle = t }
  where t0 = acThrottle s0
        t = max 0 $ min 1 $ t0 + delta / 100

parseMessage :: String -> AcState -> AcState
parseMessage ['t','h','+'] = adjustThrottle 2
parseMessage ['t','h','-'] = adjustThrottle (-2)
parseMessage ('p':'u':rest) = adjustPitch $ read rest
parseMessage ('p':'d':rest) = adjustPitch $ (-1) * read rest
parseMessage ('p':rest) = \a -> a { acPitch = degToRad $ read rest }
parseMessage ('t':rest) = \a -> a { acThrottle = read rest / 100 }
parseMessage ('h':rest) =  newHeading $ read rest
parseMessage ('r':rest) =  turn $ read rest
parseMessage ('l':rest) = turn $ (-1) * read rest
parseMessage _ = id

port :: Int
port = 8000

main :: IO ()
main = do
  var <- newTVarIO startState
  _ <- forkIO $ simpleSim var
  putStrLn $ "Starting on port " ++ show port
  WS.runServer "127.0.0.1" port (app var)

app :: TVar AcState -> WS.ServerApp
app var pending = do
  putStrLn "Got pending connection."
  conn <- WS.acceptRequest pending
  let send = do
        ac <- readTVarIO var
        let z = acAltitude ac
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
  _ <- forkIO $ forever send
  let receive = do
        txt <- unpack <$> WS.receiveData conn
        putStrLn $ "Received " ++ txt
        let u = parseMessage txt
        atomically $ modifyTVar var u
  forever receive
  -- FIXME if we use 'forever' here, what happens when the connection closes?
