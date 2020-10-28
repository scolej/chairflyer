module Happy where

import NVector
import Vec
import Data.List
import Data.Maybe
import System.Exit
import Text.Printf

-- The tiniest test framework.

data Test =
  Test { testDescription :: String
       , testResult :: Maybe [String]
       }

(@@@) :: String -> Maybe [String] -> Test
infixr 1 @@@
(@@@) = Test

-- FIXME Maybe description should be a list of strings.
prefixTests :: String -> [Test] -> [Test]
prefixTests p ts = map f ts
  where f t = let d0 = testDescription t
              in t { testDescription = p ++ ": " ++ d0 }

-- | Add some indentation to every line.
indent :: Int -> [String] -> [String]
indent i = map ((++) (take i $ repeat ' '))

testResultStr :: Test -> String
testResultStr t =
  let d = testDescription t
  in case (testResult t) of
      Nothing -> "☻"
      Just ms -> intercalate "\n" (d : indent 2 ms)

-- | Join/separate lines depending on how long they are.
-- Lines with only one character go on the same line,
-- otherwise we add a separator.
spreadPrint :: [String] -> [String]
spreadPrint ss = go (0 :: Int) ss
  where
    go _ [] = []
    go _ (a:[]) = [a]
    go i (a:b:xs)
      | i > 100 = "\n" : go 0 (b:xs)
      | length a == 1 && length b == 1 = a : go (i + 1) (b:xs)
      | otherwise = a : "\n\n" : go (i + 1) (b:xs)

-- | Pad columns to make a table.
padCols
  :: [Int]      -- ^ Indices of columns to right align
  -> [[String]] -- ^ Rows of columns
  -> [[String]] -- ^ Rows of padded columns
padCols ris rows =
  let rowsp = map (\cs -> cs ++ repeat "") rows -- append blank columns
      l = maximum $ map length rows             -- most columns in any row
      g i xs = xs !! i                          -- FIXME
      p :: Int -> String -> String              -- given a column index pad a string
      p i s =
        let w = maximum $
                map length (map (g i) rowsp)    -- width of column
            pad = take (w - length s) (repeat ' ')
        in if i `elem` ris
           then pad ++ s
           else s ++ pad
  in map (zipWith ($) (map p [0..l])) rows

-- TODO
-- need to output some newlines lines don't get too long and Emacs doesn't break

runTests :: [Test] -> IO ()
runTests ts = do
  let failed = any (isJust . testResult) ts
      ls = spreadPrint $ map testResultStr ts ++ [if failed then "FAIL" else "PASS"]
  mapM_ putStr ls
  putStrLn ""
  if failed
  then exitFailure
  else exitSuccess

eqDoub :: Double -> Double -> Double -> Bool
eqDoub tol a b = abs (a - b) < tol

assertDouble :: Double -> Double -> Double -> Maybe [String]
assertDouble tol e a =
  if eqDoub tol e a
  then Nothing
  else Just [ "actual value was not within tolerance of expected value"
            , printf "tolerance        : %10.5e" tol
            , printf "expected (lower) : %10.5e" (e - tol)
            , printf "expected (middle): %10.5e" e
            , printf "expected (upper) : %10.5e" (e + tol)
            , printf "actual           : %10.5e" a
            ]

eqLL :: LatLon -> LatLon -> Bool
eqLL (alat, alon) (blat, blon) =
  eqDoub 1e-5 alat blat &&
  eqDoub 1e-5 alon blon

assertV3 :: Double -> Vec3 -> Vec3 -> Maybe [String]
assertV3 tol (Vec3 ex ey ez) (Vec3 ax ay az) =
  let f n e a =
        let eq = d < tol
            d = abs (e - a)
        in ( eq
           , [ if eq then "" else "!"
             , n
             , printf "%10.4e" e
             , printf "%10.4e" a
             , if eq then "" else printf "%10.4e" d
             ]
           )
      cs = [ f "x" ex ax
           , f "y" ey ay
           , f "z" ez az
           ]
      hs = ["", "", "expected", "actual", "difference"]
  in if any (not . fst) cs
     then Just $ map (unwords) $ padCols [2, 3, 4] $ hs : (map snd cs)
     else Nothing

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

isNegative :: (Ord a, Num a, Show a) => a -> Maybe [String]
isNegative a
  | a < 0 = Nothing
  | otherwise =
      Just [ "expected negative value:"
           , show a
           ]

magIncreasing :: (Ord a, Num a, Show a) => a -> a -> Maybe [String]
magIncreasing a b
  | abs a < abs b = Nothing
  | otherwise =
      Just [ "expected increasing magnitudes:"
           , show a
           , show b
           ]

-- FIXME specialise for double? to get numbers formatted the same
magDecreasing :: (Ord a, Num a, Show a) => a -> a -> Maybe [String]
magDecreasing a b
  | abs a > abs b = Nothing
  | otherwise =
      Just [ "expected decreasing magnitudes:"
           , show a
           , show b
           ]
