module Happy where

import NVector
import Data.List
import Data.Maybe
import System.Exit
import Text.Printf

-- The tiniest test framework.

data Test =
  Test { testDescription :: String
       , testResult :: Maybe [String]
       }

-- | Add some indentation to every line.
indent :: Int -> [String] -> [String]
indent i = map ((++) (take i $ repeat ' '))

go :: Test -> String
go t =
  let d = testDescription t
  in case (testResult t) of
      Nothing -> "☻"
      Just ms -> intercalate "\n" (d : indent 2 ms)

-- | Join/separate lines depending on how long they are.
-- Lines with only one character go on the same line,
-- otherwise we add a separator.
spreadPrint :: [String] -> [String]
spreadPrint [] = []
spreadPrint (a:b:xs)
  | length a == 1 && length b == 1 = a : spreadPrint (b:xs)
  | otherwise = a : "\n\n" : spreadPrint (b:xs)
spreadPrint (a:[]) = [a]

runTests :: [Test] -> IO ()
runTests ts = do
  let failed = any (isJust . testResult) ts
      ls = spreadPrint $ map go ts ++ [if failed then "FAIL" else "PASS"]
  putStrLn ""
  mapM_ putStr ls
  putStrLn ""
  if failed
  then exitFailure
  else exitSuccess

eqDoub :: Double -> Double -> Double -> Bool
eqDoub tol a b = abs (a - b) < tol

eqLL :: LatLon -> LatLon -> Bool
eqLL (alat, alon) (blat, blon) =
  eqDoub 1e-5 alat blat &&
  eqDoub 1e-5 alon blon

-- | Describe a mismatch of expected and actual values.
mismatch :: String -> String -> [String]
mismatch e a =
  ["expected value was: " ++ e
  ,"      actual value: " ++ a
  ]

assertLL :: LatLon -> LatLon -> Maybe [String]
assertLL a@(alat, alon) b@(blat, blon) =
  if eqLL a b
  then Nothing
  else Just $ mismatch
               (printf "%16.10f %16.10f" alat alon)
               (printf "%16.10f %16.10f" blat blon)

