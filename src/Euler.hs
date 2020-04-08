module Euler where

eulerStep ::
     (Double -> a -> b) -- ^ How to compute the rate of change from its state and a time step
  -> (a -> b -> Double -> a) -- ^ How to linearly add the rate and the state with a time scale
  -> Double -- ^ Time step
  -> a -- ^ Initial state
  -> a -- ^ Next state
eulerStep fr lstep dt s0 =
  lstep s0 (fr 0 s0) dt
