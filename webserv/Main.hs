{-# LANGUAGE DeriveGeneric #-}

import qualified Network.WebSockets as WS
import Data.Text
import Data.Aeson
import GHC.Generics
import AcSystem
import Integrators
import Units
import Vec
import Atmosphere
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad

--
-- Simulation
--

threadDelaySec :: Double -> IO ()
threadDelaySec s = threadDelay . round $ s * 1000000

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
            , acPitch = degToRad 10
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
           } deriving (Generic, Show)

instance ToJSON Response where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Response

port :: Int
port = 8000

main :: IO ()
main = do
  var <- newTVarIO startState
  forkIO $ simpleSim var
  putStrLn $ "Starting on port " ++ show port
  WS.runServer "127.0.0.1" port (app var)

app :: TVar AcState -> WS.ServerApp
app var pending = do
  putStrLn "Got pending connection."
  conn <- WS.acceptRequest pending
  let send = do
        s <- atomically $ readTVar var
        let Vec3 _ _ z = acPos s
            v = acVel s
            resp = Response { rAltitude = mToFt z
                            , rAirspeed = mpsToKnots (magv2 v)}
        WS.sendTextData conn $ encode resp
        threadDelaySec simTimeStep
  forever send
