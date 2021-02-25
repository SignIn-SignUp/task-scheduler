{-# LANGUAGE NoImplicitPrelude #-}
module Constraints
(
    Constraints(..)
) where




import           GHC.Base (Bool (False, True), Eq ((==)), (.))
import           GHC.Num  (Integer)

infixl 2 \#\


infixl 0 ?

infixl 1 :?


data Cond a = a :? a

(?) :: Bool -> Cond a -> a
True ? (x :? _)  = x
False ? (_ :? y) = y

class Constraints a where
  {-# MINIMAL select, size, conflicts, (without | (\#\)) #-}

  -- |  Retruns the conflicting 'Constraints' of\\
  --    a and b.
  --    conflicts is commutative:
  --
  --    prop> conflicts a b =  conflicts b a
  conflicts :: a -> a -> a

  -- |  Retruns the size.\\
  --    (Number of contained elements)
  size :: a -> Integer

  -- |  Returns minimal 'Constraints'.
  select :: a -> a

  -- |  Returns minimal 'Constraints'
  --    considdering ones of the list.
  minimize :: a -> [a] -> a
  minimize a [] = select a
  minimize a (x:xs) = null r ? select s :? r
    where r = minimize s xs
          s = a \#\ x

  -- |  Checks if size > 0
  null :: a -> Bool
  null = (==) 0 . size

  -- |  Removes conflicting Constrains in b from a.\\
  --    Is a synonym for '\#\'
  without :: a -> a -> a
  without a b = a \#\ b

  -- | Is a synonym for 'without'
  (\#\) :: a -> a -> a
  (\#\) a b = a `without` b

