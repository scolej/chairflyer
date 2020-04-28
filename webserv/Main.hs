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
import qualified Network.WebSockets as WS

--
-- Simulation
--

threadDelaySec :: Double -> IO ()
threadDelaySec s = threadDelay . round $ s * 1000000

-- | Individual simulation time steps, seconds.
simTimeStep :: Double
simTimeStep = 0.2

simStep :: AcSystem -> AcSystem
simStep = stepAcSystem simTimeStep

startState :: AcSystem
startState =
    let ac0 =
          AcState { acTime = 0
                  , acPos = zerov3
                  , acVel = Vec2 0 0
                  , acMass = acpMass hackyJab
                  , acHeading = 0
                  , acPitch = degToRad 10
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

app :: TVar AcSystem -> WS.ServerApp
app var pending = do
  putStrLn "Got pending connection."
  conn <- WS.acceptRequest pending
  let send = do
        s <- atomically $ readTVar var
        let ac = sysState s
            Vec3 _ _ z = acPos ac
            v = acVel ac
            resp = Response { rAltitude = mToFt z
                            , rAirspeed = mpsToKnots (magv2 v)}
        WS.sendTextData conn $ encode resp
        threadDelaySec simTimeStep
  forever send
