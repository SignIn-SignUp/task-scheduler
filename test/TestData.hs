{-# LANGUAGE FlexibleInstances #-}
module TestData
(
    ringTestData,
    starTestData,
    singletonsTestData,
    forestTestData
) where

import ConstraintsImpl (TestAssingnments(..))
import Scheduler.Assignment (Assignment(..))
import Scheduler.Constraints (Constraints)
import Scheduler.Like (Like(..))
import Test.Tasty.QuickCheck (Arbitrary(arbitrary))

instance Like Integer where
  (~~) a b = a == b

instance Arbitrary (Assignment Integer TestAssingnments) where
    arbitrary = do AS <$> arbitrary <*> arbitrary



ringTestData :: [Assignment Integer TestAssingnments]
ringTestData =
    [
        AS 1 (TA [1,2,3])
    ,   AS 2 (TA [3,4,5])
    ,   AS 3 (TA [5,6])
    ,   AS 4 (TA [6,7])
    ,   AS 5 (TA [7,8,9])
    ,   AS 6 (TA [1,9])
    ]



starTestData :: [Assignment Integer TestAssingnments]
starTestData =
    [
        AS 1 (TA [1,2,3,4,5])
    ,   AS 2 (TA [1,7])
    ,   AS 3 (TA [2,8])
    ,   AS 4 (TA [3,9])
    ,   AS 5 (TA [4,10])
    ,   AS 6 (TA [5,11])
    ]


singletonsTestData :: [Assignment Integer TestAssingnments]
singletonsTestData =
    [
        AS 1 (TA [1,2])
    ,   AS 2 (TA [3,4])
    ,   AS 3 (TA [5,6])
    ,   AS 4 (TA [7,9,10])
    ,   AS 5 (TA [8,11])
    ,   AS 6 (TA [15])
    ]


forestTestData :: [Assignment Integer TestAssingnments]
forestTestData =
    [
        AS 1 (TA [1,2,3])
    ,   AS 2 (TA [3,5,6])
    ,   AS 3 (TA [10,11])
    ,   AS 4 (TA [11])
    ,   AS 5 (TA [11,12,13])
    ,   AS 6 (TA [13])
    ,   AS 7 (TA [27])
    ]

conflictingTestData :: [Assignment Integer TestAssingnments]
conflictingTestData =
    [
        AS 1 (TA [1,2])
    ,   AS 2 (TA [1,2])
    ,   AS 3 (TA [2,1])
    ,   AS 4 (TA [2,1])
    ]

