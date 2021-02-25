# task-scheduler

This library provides **schedule** in [*Scheduler.hs*](src/Scheduler.hs) 
that can create a Schedule of given assignments and compliant with the respective constraints.

## Algorithm
This library will implement a greeedy naive algorithm at first. Improvment
of the algorithm is welcome.

## Usage

To be able to use the ```schedule```method from [*Scheduler.hs*](src/Scheduler.hs) the type
**Assignment** must be used.
```{haskell}
data Assignment i c
  = -- |  An Assingment has:
    --
    -- * id = **i**
    -- * constraints = **c**
    AS i c
```
Also the **i** and **c** musst provice instances of **Ord** and **c** of **Constraint**
from [*Constraints.hs*](src/Constraints.hs).
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
```

**minimize** and primarily **select** will be used to determine which constraints
will be selected and the priority of constraints.

# TODO :eyes:

- [ ] :straight_ruler: improve algorithm 
- [ ] :memo: extend README 
- [ ] :sunny: add and improve tests 
