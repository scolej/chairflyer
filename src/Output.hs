module Output where

import Text.Printf

sci :: Double -> String
sci = printf "%15.5e"

writeData :: String -> [[String]] -> IO ()
writeData file xs =
  writeFile file $ unlines $ map unwords xs
