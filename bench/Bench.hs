{-# LANGUAGE FlexibleInstances #-}
module Bench where

import ConstraintsBenchImpl
import Criterion.Main
import Scheduler.Assignment (Assignment(AS), resolve)
import Scheduler.Like (Like(like))
import Test.Tasty.QuickCheck (Arbitrary(arbitrary), generate, vectorOf)


main :: IO ()
main = defaultMain [
  bgroup "resolve"
    [ bench "resolve length 50" $  whnfAppIO (\n -> resolve <$> (sampleList n :: IO [Assignment Integer TestBenchConstraints])) 50
    , bench "resolve length 100" $  whnfAppIO (\n -> resolve <$> (sampleList n :: IO [Assignment Integer TestBenchConstraints])) 100
    , bench "resolve length 150" $  whnfAppIO (\n -> resolve <$> (sampleList n :: IO [Assignment Integer TestBenchConstraints])) 150
    , bench "resolve length 200" $  whnfAppIO (\n -> resolve <$> (sampleList n :: IO [Assignment Integer TestBenchConstraints])) 200
    , bench "resolve length 300" $  whnfAppIO (\n -> resolve <$> (sampleList n :: IO [Assignment Integer TestBenchConstraints])) 300
    , bench "resolve length 400" $  whnfAppIO (\n -> resolve <$> (sampleList n :: IO [Assignment Integer TestBenchConstraints])) 400
    , bench "resolve length 500" $  whnfAppIO (\n -> resolve <$> (sampleList n :: IO [Assignment Integer TestBenchConstraints])) 500
    ]
  ]

instance Arbitrary (Assignment Integer TestBenchConstraints) where
    arbitrary = do AS <$> arbitrary <*> arbitrary

instance Like Integer where
    like = (==)

sampleList :: Arbitrary a => Int -> IO [a]
sampleList = generate . flip vectorOf arbitrary
