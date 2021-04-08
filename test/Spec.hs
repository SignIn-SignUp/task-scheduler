{-# LANGUAGE ScopedTypeVariables #-}

module Spec
(
  main
) where
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Test.Tasty.QuickCheck as QC (testProperty, (==>))
-- import           Test.Tasty.SmallCheck as SC (testProperty, (==>)) maybe nclude smallcheck tests
import ConstraintsImpl (TestAssingnments, hasConflicts)
import Data.List (sort)
import Scheduler.Assignment (Assignment(AS), resolve)
import qualified Scheduler.Constraints as C
       (Constraints(conflicts, minimize, null, select, without, (\#\)))
import Scheduler.Like (Like((~~)))
import TestData (ringTestData, singletonsTestData)

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [qc]

qc :: TestTree
qc = testGroup "QuickCheck" [qcConstraints,qcAssignment]

qcConstraints :: TestTree
qcConstraints = testGroup "Constraints"
  [
    QC.testProperty "conflicts a b == conflicts b a" $
      \ta1 ta2 -> C.conflicts (ta1 :: TestAssingnments) (ta2::TestAssingnments) == C.conflicts ta1 ta2
  , QC.testProperty "wihtout a b == a \\#\\ b" $
      \ta1 ta2 -> C.without (ta1::TestAssingnments) (ta2::TestAssingnments) == (ta1 C.\#\ ta2)
  , QC.testProperty "select a == minimize a []" $
      \a -> C.select (a::TestAssingnments) == C.minimize a [] []
  ]

qcAssignment :: TestTree
qcAssignment = testGroup "Assignment"
  [
    QC.testProperty "resolve a should contain the same elments as resolve $ resolve a" $
      \(ts::[Assignment Integer TestAssingnments]) -> sort (resolve ts) == sort (resolve $ resolve ts)
  , QC.testProperty "colficts in (resolve a) == []" $
      \(ts::[Assignment Integer TestAssingnments]) -> not . hasConflicts . map (\(AS _ c) -> c) $ resolve ts
  ]


unitTests :: TestTree
unitTests = testGroup "Unit tests" [schedulerUnit]

schedulerUnit :: TestTree
schedulerUnit = testGroup "Assignment"
  [
    testCase "Scheduled comparison ring (same Length)" $
      length (resolve ringTestData) @?= length ringTestData
  , testCase "Scheduled comparison singletons (same Length)" $
      length (resolve singletonsTestData ) @?= length singletonsTestData
  , testCase "List comparison contradicting (not same length)" $
      assertBool "Length of scheduled contradiction was eqal" $ length (resolve singletonsTestData ) == length singletonsTestData
  ]

main :: IO ()
main = defaultMain tests
