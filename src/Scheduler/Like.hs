module Scheduler.Like
(
    Like(..)
)
where

class Like a where
    {-# MINIMAL (like | (~~)) #-}

    -- |  Is true if a is like(kind of equal to) b \\
    --    like is commutative
    --
    --    prop> like a b =  like b a
    like :: a -> a -> Bool
    like a b = a ~~ b

    -- |  '~~' is a synonym for 'like' \\
    --    '~~' is commutative
    --
    --    prop> a ~~ b =  b ~~ a
    (~~) :: a -> a -> Bool
    (~~) a b = like a b
