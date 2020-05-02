import SpringSystem2
import Controller
import Integrators
import Output

type SpringController = Controller SpringState

data Sys =
  Sys { sysState :: SpringState
      , sysController :: SpringController
      }

sInit :: SpringState
sInit =
  SpringState { sPos = 0
              , sVel = 0
              , sTime = 0
              , sForce = 0
              }

cInit :: SpringController
cInit = pidController (0.1, 0.01, 3) sPos (\x s -> s { sForce = x }) 1

sysInit =
  Sys { sysState = sInit
      , sysController = cInit
      }

stepSpring :: Double -> SpringState -> SpringState
stepSpring dt = rk4step springRate springStep dt

step :: Double -> Sys -> Sys
step dt sys0 =
  Sys { sysState = s
      , sysController = c
      }
  where s0 = sysState sys0
        Controller c0 = sysController sys0
        (s1, c) = c0 dt s0
        s = stepSpring dt s1

takeMinutes :: Double -> [Sys] -> [Sys]
takeMinutes mins = takeWhile (\s -> (sTime . sysState) s < mins * 60)

showState :: Sys -> [String]
showState sys = map sci [t, y, f]
  where s = sysState sys
        t = sTime s
        y = sPos s
        f = sForce s

hist :: [Sys]
hist = takeMinutes 2 $ iterate (step 0.1) sysInit

main :: IO ()
main = do
  writeData "out.dat" (map showState hist)
