module ConstraintsBenchImpl
(
    TestBenchConstraints(TB)
)
where


import Prelude hiding (null)
import Scheduler.Constraints
       (Constraints(conflicts, null, select, size, without, (\#\)))
import Test.Tasty.QuickCheck (Arbitrary(arbitrary), generate, vectorOf)

newtype TestBenchConstraints = TB [Integer] deriving (Show)


instance Arbitrary TestBenchConstraints where
  arbitrary = do TB <$> arbitrary


instance Constraints TestBenchConstraints where

  select (TB (x:xs)) = TB [x]
  select x           = x

  size = toInteger . length . (\(TB l) -> l)

  conflicts (TB a) (TB b) = TB $ filter (`elem` b) a

  without (TB a) (TB b) =  TB $ filter (`notElem` b) a


instance Eq TestBenchConstraints where
    (==) (TB a) (TB b) = a == b

instance Ord TestBenchConstraints where
  (<=) (TB []) (TB []) = True
  (<=) (TB []) _ = True
  (<=) _ (TB []) = False
  (<=) (TB aa@(a:as)) bb@(TB (b:bs))
    | a < b = True
    | a == b = TB as <= TB bs
    | otherwise = False
