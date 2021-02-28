import           Test.Tasty            (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit      (assertBool, testCase, (@?=))
import           Test.Tasty.QuickCheck as QC (testProperty, (==>))
-- import           Test.Tasty.SmallCheck as SC (testProperty, (==>)) maybe nclude smallcheck tests

import           Constraints           (Constraints (conflicts, minimize, select, without, (\#\)))
import           ConstraintsImpl       (TestAssingnments)
import qualified ConstraintsImpl
import           Scheduler             (schedule)
import           TestData              (ringTestData, singletonsTestData)

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [qc]

qc :: TestTree
qc = testGroup "QuickCheck" [qcConstraints]

qcConstraints :: TestTree
qcConstraints = testGroup "Constraints"
  [
    QC.testProperty "conflicts a b == conflicts b a" $
      \ta1 ta2 -> conflicts (ta1 :: TestAssingnments) (ta2::TestAssingnments) == conflicts ta1 ta2
  , QC.testProperty "wihtout a b == a \\#\\ b" $
      \ta1 ta2 -> without (ta1::TestAssingnments) (ta2::TestAssingnments) == (ta1 \#\ ta2)
  , QC.testProperty "select a == minimize a []" $
      \a -> select (a::TestAssingnments) == minimize a [] []
  ]

unitTests :: TestTree
unitTests = testGroup "Unit tests" [schedulerUnit]

schedulerUnit :: TestTree
schedulerUnit = testGroup "Schduler"
  [
    testCase "Scheduled comparison ring (same Length)" $
      length (schedule ringTestData) @?= length ringTestData
  , testCase "Scheduled comparison singletons (same Length)" $
      length (schedule singletonsTestData ) @?= length singletonsTestData
  , testCase "List comparison contradicting (not same length)" $
      assertBool "Length of scheduled contradiction was eqal" $ length (schedule singletonsTestData ) == length singletonsTestData
  ]

main :: IO ()
main = defaultMain tests
