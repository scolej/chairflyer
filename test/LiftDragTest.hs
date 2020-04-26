module LiftDragTest (sanity) where

import Happy
import Handy
import LiftDrag

ld deg = liftDrag 7.4 (degToRad deg)

sanity :: [Test]
sanity =
  let (l1, d1) = ld 5
      (l2, d2) = ld 6
      (ls, ds) = ld 15
      (lsa, dsa) = ld 16
      (nl1, nd1) = ld (-5)
      (nl2, nd2) = ld (-6)
  in
  [ "lift increases with angle of attack" @@@ magIncreasing l1 l2
  , "drag increases with angle of attack" @@@ magIncreasing d1 d2
  , "lift is negative at negative angle of attack" @@@ isNegative nl1
  , "negative lift increases with negative angle of attack" @@@ magIncreasing nl1 nl2
  , "drag is positive at negative angle of attack" @@@ Nothing
  , "drag increases with negative angle of attack" @@@ magIncreasing nd1 nd2
  , "lift decreases beyond stall" @@@ magDecreasing ls lsa
  , "drag increases beyond stall" @@@ magIncreasing ds dsa
  ]
