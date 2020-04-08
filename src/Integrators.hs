module Integrators where

type Integrator a b =
     (Double -> a -> b) -- ^ Compute the rate of change from state and a time step
  -> (Double -> b -> a -> a) -- ^ Linearly add rate and state with a time step
  -> Double -- ^ Time step
  -> a -- ^ Initial state
  -> a -- ^ Next state

eulerStep :: Integrator a b
eulerStep rate step dt s0 = step dt (rate 0 s0) s0

rk4step :: Integrator a b
rk4step rate step dt s0 = (step4 . step3 . step2 . step1) s0
  where f1 = rate 0 s0
        f2 = rate (dt / 2) (step (dt / 2) f1 s0)
        f3 = rate (dt / 2) (step (dt / 2) f2 s0)
        f4 = rate dt (step dt f3 s0)
        step1 = step (dt / 6) f1
        step2 = step (dt / 3) f2
        step3 = step (dt / 3) f3
        step4 = step (dt / 6) f4
