import SpringSystem2
import Controller
import Integrators
import Output

type SpringController = Controller SpringState PIDState

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
cInit =
  Controller { cStep = pidStep (0.1, 0.01, 3) sPos (\x s -> s { sForce = x }) 1
             , cState = pidZero
             }

sysInit =
  Sys { sysState = sInit
      , sysController = cInit
      }

stepSpring :: Double -> SpringState -> SpringState
stepSpring dt = rk4step springRate springStep dt

step :: Double -> Sys -> Sys
step dt sys0 =
  Sys { sysState = s
      , sysController = c0 { cState = c }
      }
  where s0 = sysState sys0
        c0 = sysController sys0
        (s1, c) = (cStep c0) s0 dt (cState c0)
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
