module ConstraintsImpl
(
    TestAssingnments(TA)
)
where


import           Constraints           (Constraints (conflicts, select, size, without))
import           Test.Tasty.QuickCheck (Arbitrary (arbitrary))

newtype TestAssingnments = TA [Integer] deriving (Show)

instance Arbitrary TestAssingnments where
  arbitrary = do TA <$> arbitrary



instance Constraints TestAssingnments where

  select (TA (x:xs)) = TA [x]
  select x           = x

  size = toInteger . length . (\(TA l) -> l)

  conflicts (TA a) (TA b) = TA $ filter (`elem` b) a

  without (TA a) (TA b) =  TA $ filter (`notElem` b) a


instance Eq TestAssingnments where
    (==) (TA a) (TA b) = a == b

instance Ord TestAssingnments where
  (<=) (TA a) (TA b) = a <= b
