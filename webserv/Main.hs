{-# LANGUAGE DeriveGeneric #-}

import Debug.Trace
import System.Directory
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
import qualified Data.Binary as B
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
  let h = degToRad 8
      p = llDegToNVec (-37.698329, 145.365335)
  in AcState { acTime = 0
             , acAltitude = 0
             , acPos = p
             , acHeadingV = headingVector p h
             , acHeading = h
             , acVel = Vec2 0 0
             , acMass = acpMass hackyJab
             , acPitch = degToRad 10
             -- Massive FIXME - if I make this 0 everything goes haywire
             -- div by 0 somewhere?
             , acThrottle = 0.1
             }

simpleSim :: TVar AcState -> IO ()
simpleSim var = forever go
  where f s = iterate simStep s !! 1
        go = do
          s <- readTVarIO var
          print $ radToDeg $ acHeading s
          --
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
           , rRpm :: Double
           } deriving (Generic, Show)

instance ToJSON Response where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Response

wrapHeadingRad :: Double -> Double
wrapHeadingRad r = mod' r (2 * pi)

turn :: Double -> AcState -> AcState
turn deg s0 =
  s0 { acHeadingV = hv1
     , acHeading = heading p0 hv1
     }
  where p0 = acPos s0
        hv0 = acHeadingV s0
        delta = degToRad (-deg)
        hv1 = rotV p0 delta hv0

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
parseMessage ('r':rest) =  turn $ read rest
parseMessage ('l':rest) = turn $ (-1) * read rest
parseMessage _ = id

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

app :: TVar AcState -> WS.ServerApp
app var pending = do
  putStrLn "Got pending connection."
  conn <- WS.acceptRequest pending
  let send = do
        ac <- readTVarIO var
        let z = acAltitude ac
            -- TODO only use the forward component of velocity
            v = acVel ac
            h = acHeading ac
            ll = nvecToLLDeg . acPos $ ac
            resp = Response { rAltitude = mToFt z
                            , rAirspeed = mpsToKnots (magv2 v)
                            , rLatLon = ll
                            , rHeadingRad = h
                            , rRpm = acThrottle ac * acpMaxPropRpm hackyJab
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
  let receive = do
        txt <- unpack <$> WS.receiveData conn
        putStrLn $ "Received " ++ txt
        let u = parseMessage txt
        atomically $ modifyTVar var u
  forever receive
  -- FIXME if we use 'forever' here, what happens when the connection closes?
