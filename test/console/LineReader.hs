import System.IO
import Control.Monad
import Data.Char

trim :: String -> String
trim = reverse . f . reverse . f
  where f = dropWhile (not . isAlphaNum)

-- | Simple whole line reader and writer.
-- So main app doesn't have to worry about partial lines.
main :: IO ()
main = do
  o <- openFile "input" WriteMode
  putStrLn "Ready for input!"
  forever $ do
    line <- trim <$> getLine
    unless (null line) $ do
      hPutStrLn o line
      hFlush o
