import Happy
import qualified LiftDragTest
import qualified NVectorTest
import qualified VecTest

main :: IO ()
main = do
  runTests $
    concat [ NVectorTest.tests
           , VecTest.tests
           , LiftDragTest.sanity
           ]
