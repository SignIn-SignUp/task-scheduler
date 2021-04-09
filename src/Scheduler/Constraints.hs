{-# LANGUAGE NoImplicitPrelude #-}
module Scheduler.Constraints
(
  Constraints(..)
)
where




import GHC.Base (Bool(False, True), Eq((==)), (.))
import GHC.Num (Integer)
import Prelude (Foldable)

infixl 2 \#\


infixl 0 ?

infixl 1 :?


data Cond a = a :? a

(?) :: Bool -> Cond a -> a
True ? (x :? _)  = x
False ? (_ :? y) = y

class Constraints a where
  {-# MINIMAL select, size, conflicts, (without | (\#\)) #-}

  -- |  Retruns the conflicting 'Constraints' of
  --    a and b. \\
  --    conflicts is commutative
  --
  --    prop> conflicts a b =  conflicts b a
  conflicts :: a -> a -> a

  -- |  Retruns the size. \\
  --    (Number of contained elements)
  size :: a -> Integer

  -- |  Returns minimal 'Constraints'.
  select :: a -> a

  -- |  Returns minimal 'Constraints' taking the given
  --    ones into account. \\ The default implementation
  --    only takes the colliding constraints into account. \\
  --    If the third argument should be taken into account
  --    it musst be implemented. \\ The third argument are the
  --    similar remaining entries.
  minimize :: (Foldable f) => a -> [a] -> f a -> a
  minimize a [] _ = select a
  minimize a (x:xs) f = null r ? select sn :? r
    where r = minimize s xs f
          sn = null s ? a :? s
          s = a \#\ x

  -- |  Checks if size == 0
  null :: a -> Bool
  null = (==) 0 . size

  -- |  Removes conflicting Constrains in b from a. \\
  --    Is a synonym for '\#\'
  without :: a -> a -> a
  without a b = a \#\ b

  -- | Is a synonym for 'without'
  (\#\) :: a -> a -> a
  (\#\) a b = a `without` b
