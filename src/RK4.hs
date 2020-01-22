module RK4 where

rk4step ::
     (b -> a -> a) -- ^ How to add the rate of change type and the state type
  -> (Double -> b -> b) -- ^ How to scale a rate of change
  -> (b -> b -> b) -- ^ How to add rates of change
  -> (Double -> a -> b) -- ^ How to compute the instantaneous rate of change of the system.
  -> Double -- ^ Time step
  -> a -- ^ Initial state
  -> a -- ^ Next state
rk4step sa rs ra f dt s0 = sa (rs (dt / 6) r) s0
  where
    r = foldl1 ra [f1, rs 2 f2, rs 2 f3, f4]
    f1 = f 0 s0
    f2 = f (dt / 2) (sa (rs (dt / 2) f1) s0)
    f3 = f (dt / 2) (sa (rs (dt / 2) f2) s0)
    f4 = f dt (sa (rs dt f3) s0)
