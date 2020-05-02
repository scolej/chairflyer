import Happy
import qualified LiftDragTest
import qualified NVectorTest

main :: IO ()
main = do
  runTests $
    concat [ NVectorTest.tests
           , LiftDragTest.sanity
           ]
