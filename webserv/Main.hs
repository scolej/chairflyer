{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}

import SimState
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Exception
import Data.Aeson
import Data.Fixed
import Data.Text hiding (unlines)
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
simTimeStep = 1

ylil :: NVec
ylil = llDegToNVec (-37.698329, 145.365335)

atmos :: NVec -> Double -> Atmosphere
atmos =
  let e = localEast ylil
      v = scalev3 (knotsToMps 5) e
      w = const v
  in isaWind w

data Controller a b = Controller
  { conSimState :: a -> SimState
  , conInput :: FromJSON b => b -> a -> a
  , conTimeStep :: Double -> a -> a
  }

simpleSim :: Controller a b -> TVar a -> IO ()
simpleSim c var = forever go
  where f = (conTimeStep c) simTimeStep
        go = do
          atomically $ modifyTVar var f
          threadDelaySec simTimeStep

--
-- Jab
--

data JabConf =
  Landed | Climb | Cruise | FastCruise | Descent
  deriving Generic

instance B.Binary JabConf
instance FromJSON JabConf
instance ToJSON JabConf

data JabState = JabState
  { jsSim :: SimState
  , jsConf :: JabConf
  , jsFuel :: Double
  , jsRpm :: Double
  } deriving (Generic)

instance B.Binary JabState

startState :: JabState
startState =
  let h = degToRad 8
      p = ylil
      ss = SimState
        { ssTime = 0
        , ssAltitude = 0
        , ssHeadingV = headingVector p h
        , ssPosition = p
        , ssVelocity = zerov2
        }
  in JabState { jsSim = ss
              , jsConf = Landed
              , jsFuel = 135
              , jsRpm = 0
              }

data JabCommand
  = Turn Double                -- ^ turn by some amount of degrees
  | AdoptConfiguration JabConf -- ^ change flight configuration
  | FastForward Double         -- ^ fast-forward the clock
  deriving Generic

instance FromJSON JabCommand
instance ToJSON JabCommand

handleJabCommand :: JabCommand -> JabState -> JabState
handleJabCommand (AdoptConfiguration Landed) js =
  let jsSim' = (jsSim js) { ssVelocity = zerov2 }
  in js { jsRpm = 0
        , jsSim = jsSim'
        }
handleJabCommand (AdoptConfiguration Climb) js =
  let jsSim' = (jsSim js) { ssVelocity = Vec2 (knotsToMps 80) (fpmToMps 1000) }
  in js { jsRpm = 3100
        , jsSim = jsSim'
        }
handleJabCommand (AdoptConfiguration Cruise) js =
  let jsSim' = (jsSim js) { ssVelocity = Vec2 (knotsToMps 95) 0 }
  in js { jsRpm = 2800
        , jsSim = jsSim'
        }
handleJabCommand (AdoptConfiguration Descent) js =
  let jsSim' = (jsSim js) { ssVelocity = Vec2 (knotsToMps 95) (fpmToMps $ -500) }
  in js { jsRpm = 2400
        , jsSim = jsSim'
        }
handleJabCommand (Turn deg) js =
  let ss0 = jsSim js
      p0 = ssPosition ss0
      h0 = ssHeading ss0
      hv1 = headingVector p0 (h0 + degToRad deg)
      ss1 = ss0 { ssHeadingV = hv1 }
  in js { jsSim = ss1 }
handleJabCommand (FastForward dt) js =
  let ss0 = jsSim js
      ss1 = simStep atmos dt ss0
  in js { jsSim = ss1 }
handleJabCommand _ js = js

jabStep :: Double -> JabState -> JabState
jabStep dt js =
  js { jsFuel = fuel'
     , jsSim = sim'
     }
  where sim = jsSim js
        sim' = simStep atmos dt sim
        fuelRate = hourlyToPerSecond $
          case (jsConf js) of Landed -> 0
                              Climb -> 22
                              Cruise -> 18
                              FastCruise -> 20
                              Descent -> 10
        fuel = jsFuel js
        fuel' = fuel - dt * fuelRate

jabControl :: Controller JabState JabCommand
jabControl = Controller
  { conSimState = jsSim
  , conInput = handleJabCommand
  , conTimeStep = jabStep
  }

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
  _ <- forkIO $ (simpleSim jabControl) var
  putStrLn $ "listening on port " ++ show port
  WS.runServer "127.0.0.1" port (app jabControl var)

-- TODO
-- send wait is not synced with update rate,
-- would be better to send on change

-- TODO
-- in landed conf, wind should not blow us along

app :: (B.Binary a, FromJSON b) => Controller a b -> TVar a -> WS.ServerApp
app controller var pending = do
  conn <- WS.acceptRequest pending
  putStrLn "accepted connection"
  let send = do
        cs <- readTVarIO var
        let s = (conSimState controller) cs
            z = ssAltitude s
            v = ssVelocity s
            h = heading (ssPosition s) (ssHeadingV s)
            ll = nvecToLLDeg . ssPosition $ s
            -- TODO how to add extra stuff from a
            resp = Response { rAltitude = mToFt z
                            , rAirspeed = mpsToKnots (magv2 v)
                            , rLatLon = ll
                            , rHeadingRad = h
                            , rHeadingMagRad = h - degToRad 12 -- FIXME
                            , rRpm = 0 -- FIXME
                            }
        WS.sendTextData conn (encode resp)
        -- TODO stop sending on exception
        -- catch (WS.sendTextData conn (encode resp))
          -- ((\e -> do
          -- putStrLn $ "exception while sending message: " ++ show e) :: WS.ConnectionException -> IO ())
        threadDelaySec simTimeStep
  _ <- forkIO $ forever send
  let save = do
        ac <- readTVarIO var
        B.encodeFile saveFile ac
        putStrLn "saved"
        threadDelaySec 20
  _ <- forkIO $ forever save
  let receive = do
        txt <- WS.receiveData conn
        let cmd = eitherDecode txt
        case cmd
          of Right c -> atomically $ modifyTVar var ((conInput controller) c)
             Left e -> putStrLn $ unlines ["received malformed command:", e, show txt]
  forever receive
  -- FIXME if we use 'forever' here, what happens when the connection closes?
