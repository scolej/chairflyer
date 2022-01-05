{-# LANGUAGE DeriveGeneric #-}

import SimState
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Exception
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
simTimeStep = 5

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
     , ssVelocity = Vec2 (knotsToMps 50) 3
     }

simpleSim :: TVar SimState -> IO ()
simpleSim var = forever go
  where f = simStep atmos simTimeStep
        go = do
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
        then putStrLn "resuming from save" >> B.decodeFile saveFile
        else putStrLn "starting afresh" >> return startState
  var <- newTVarIO s0
  _ <- forkIO $ simpleSim var
  putStrLn $ "starting on port " ++ show port
  WS.runServer "127.0.0.1" port (app var)

app :: TVar SimState -> WS.ServerApp
app var pending = do
  conn <- WS.acceptRequest pending
  putStrLn "accepted connection"
  let send = do
        s <- readTVarIO var
        let z = ssAltitude s
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
        catch (WS.sendTextData conn (encode resp))
          ((\e -> do
            putStrLn $ "exception while sending message: " ++ show e) :: WS.ConnectionException -> IO ())
        threadDelaySec simTimeStep
  _ <- forkIO $ forever send
  let save = do
        ac <- readTVarIO var
        B.encodeFile saveFile ac
        putStrLn "saved"
        threadDelaySec 20
  _ <- forkIO $ forever save
  let receive = do
        txt <- unpack <$> WS.receiveData conn
        putStrLn $ "received " ++ txt
        -- let u = parseMessage txt
        -- atomically $ modifyTVar var u
  forever receive
  -- FIXME if we use 'forever' here, what happens when the connection closes?
