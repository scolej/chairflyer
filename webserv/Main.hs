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
                  , acThrottle = 1
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
           } deriving (Generic, Show)

instance ToJSON Response where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Response

newHeading :: Double -> AcState -> AcState
newHeading deg s0 = s0 { acTrack = (p, h, 0) }
  where (p0, h0, d) = acTrack s0
        p = destination p0 h0 d
        h = degToRad deg

parseMessage :: String -> AcSystem -> AcSystem
parseMessage ('p':rest) = updateState (\a -> a { acPitch = degToRad $ read rest })
parseMessage ('t':rest) = updateState (\a -> a { acThrottle = read rest })
parseMessage ('h':rest) = updateState (newHeading $ read rest)
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
