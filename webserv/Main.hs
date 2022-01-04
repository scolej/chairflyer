{-# LANGUAGE DeriveGeneric #-}

import SimState
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Aeson
import Data.Fixed
import Data.Text
import GHC.Generics
import Atmosphere
import Handy
import NVector
import System.Directory
import Units
import Vec
import qualified Data.Binary as B
import qualified Network.WebSockets as WS

--
-- Simulation
--

threadDelaySec :: Double -> IO ()
threadDelaySec s = threadDelay . round $ s * 1000000

-- | Individual simulation time steps, seconds.
simTimeStep :: Double
simTimeStep = 3

ylil :: NVec
ylil = llDegToNVec (-37.698329, 145.365335)

atmos :: NVec -> Double -> Atmosphere
atmos =
  let e = localEast ylil
      v = scalev3 (knotsToMps 5) e
      w = const v
  in isaWind w

startState :: SimState
startState =
  let h = degToRad 8
      p = ylil
  in SimState
     { ssTime = 0
     , ssAltitude = 0
     , ssHeadingV = headingVector p h
     , ssPosition = p
     , ssVelocity = Vec2 0 0
     }

simpleSim :: TVar SimState -> IO ()
simpleSim var = forever go
  where f s = simStep atmos simTimeStep s
        go = do
          -- FIXME
          -- s <- readTVarIO var
          -- print . acAltitude $ s
          atomically $ modifyTVar var f
          threadDelaySec simTimeStep

--
-- Server
--

data Response =
  Response { rAltitude :: Double
           , rAirspeed :: Double
           , rLatLon :: LatLon
           , rHeadingRad :: Double
           , rHeadingMagRad :: Double
           , rRpm :: Double
           } deriving (Generic, Show)

instance ToJSON Response where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Response

port :: Int
port = 8000

saveFile :: String
saveFile = "save.dat"

main :: IO ()
main = do
  saveFileExists <- doesFileExist saveFile
  s0 <- if saveFileExists
        then B.decodeFile saveFile
        else return startState
  var <- newTVarIO s0
  _ <- forkIO $ simpleSim var
  putStrLn $ "Starting on port " ++ show port
  WS.runServer "127.0.0.1" port (app var)

app :: TVar SimState -> WS.ServerApp
app var pending = do
  putStrLn "Got pending connection."
  conn <- WS.acceptRequest pending
  let send = do
        s <- readTVarIO var
        let z = ssAltitude s
            -- TODO only use the forward component of velocity
            v = ssVelocity s
            h = heading (ssPosition s) (ssHeadingV s)
            ll = nvecToLLDeg . ssPosition $ s
            resp = Response { rAltitude = mToFt z
                            , rAirspeed = mpsToKnots (magv2 v)
                            , rLatLon = ll
                            , rHeadingRad = h
                            , rHeadingMagRad = h - degToRad 12 -- FIXME
                            , rRpm = 0 -- FIXME
                            }
        WS.sendTextData conn $ encode resp
        threadDelaySec simTimeStep
  _ <- forkIO $ forever send
  let save = do
        ac <- readTVarIO var
        B.encodeFile saveFile ac
        putStrLn "saved"
        threadDelaySec 20
  _ <- forkIO $ forever save
  return ()
  -- let receive = do
  --       txt <- unpack <$> WS.receiveData conn
  --       putStrLn $ "Received " ++ txt
  --       let u = parseMessage txt
  --       atomically $ modifyTVar var u
  -- forever receive
  -- FIXME if we use 'forever' here, what happens when the connection closes?
