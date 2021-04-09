module ConstraintsImpl
(
    TestAssingnments(TA)
  , hasConflicts
  , removeConflictsSimple
  , removeConflictsExpensive
)
where


import Data.List (sort)
import Prelude
import qualified Scheduler.Constraints as C
       (Constraints(conflicts, null, select, size, without, (\#\)))
import Test.Tasty.QuickCheck (Arbitrary(arbitrary))

newtype TestAssingnments = TA [Integer] deriving (Show)


instance Arbitrary TestAssingnments where
  arbitrary = do TA <$> arbitrary



instance C.Constraints TestAssingnments where

  select (TA (x:xs)) = TA [x]
  select x           = x

  size = toInteger . length . (\(TA l) -> l)

  conflicts (TA a) (TA b) = TA $ filter (`elem` b) a

  without (TA a) (TA b) =  TA $ filter (`notElem` b) a


instance Eq TestAssingnments where
    (==) (TA a) (TA b) = a == b

instance Ord TestAssingnments where
  (<=) (TA []) (TA []) = True
  (<=) (TA []) _ = True
  (<=) _ (TA []) = False
  (<=) (TA aa@(a:as)) bb@(TA (b:bs))
    | a < b = True
    | a == b = TA as <= TA bs
    | otherwise = False



hasConflicts :: C.Constraints a => [a] -> Bool
hasConflicts [] = False
hasConflicts (a:as) = not (all (C.null . C.conflicts a) as) || hasConflicts as

removeConflictsSimple :: (C.Constraints a, Ord a) => [a] -> [a]
removeConflictsSimple lst = if null dropped then [] else removeConflictsSimple' dropped
  where dropped = dropWhile C.null lst

removeConflictsSimple' :: C.Constraints a => [a] -> [a]
removeConflictsSimple' [] = []
removeConflictsSimple' (a:as) = a : (removeConflictsSimple' . filter (not . C.null) . map (C.\#\a)) as




removeConflictsExpensive :: (C.Constraints a, Ord a) => [a] -> [a]
removeConflictsExpensive [] = []
removeConflictsExpensive lst = if null sorted then [] else removeConflictsExpensive' sorted
  where sorted = dropWhile C.null $ sort lst

removeConflictsExpensive' :: (C.Constraints a, Ord a) => [a] -> [a]
removeConflictsExpensive' [] = []
removeConflictsExpensive' (a:as) = a : (removeConflictsExpensive' . sort . filter (not . C.null) . map (C.\#\a)) as



