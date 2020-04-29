module Controller where

-- | A function representing a controller.
type ControlStep s c = s -> Double -> c -> (s, c)

-- | A controller with a stepping function and some state.
data Controller s c =
  Controller { cStep :: ControlStep s c
             , cState :: c
             }

-- | A controller which does absolutely nothing.
idController :: Controller s Double
idController =
  Controller { cStep = \s _ _ -> (s, 0)
             , cState = 0
             }