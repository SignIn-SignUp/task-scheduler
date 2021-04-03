# task-scheduler

This library provides **resolve** in [*Assignment.hs*](src/Scheduler/Assignment.hs) 
that can create a conflict free collection of given assignments, compliant with the respective constraints.

## Algorithm
This library will implement a greeedy naive algorithm at first. Improvment
of the algorithm is welcome.

## Usage

To be able to use the ```resolve```method from [*Assignment.hs*](src/Scheduler/Assignment.hs) the type
**Assignment** must be instancieated.
```{haskell}
data Assignment i c
  = -- |  An Assingment has:
    --
    -- * id = **i**
    -- * constraints = **c**
    AS i c
```
Also the **i** and **c** musst provice instances of **Ord** and **c** of **Constraint**
from [*Constraints.hs*](src/Scheduler/Constraints.hs).
```{haskell}
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

  -- |  Returns minimal 'Constraints' taking the given\\
  --    ones into account. The default implementation\\
  --    only takes the colliding constraints into account.\\
  --    If the third argument should be taken into account\\
  --    it musst be implemented.
  minimize :: (Foldable f) => a -> [a] -> f a -> a
  minimize a [] _ = select a
  minimize a (x:xs) f = null r ? select s :? r
    where r = minimize s xs f
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
```

**minimize** and primarily **select** will be used to determine which constraints
will be selected and the priority of constraints.

# TODO :eyes:

- [ ] :straight_ruler: improve algorithm 
- [ ] :memo: extend README 
- [ ] :sunny: add and improve tests 
