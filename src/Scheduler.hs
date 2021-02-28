{-# LANGUAGE ExistentialQuantification #-}

module Scheduler
  ( schedule,
    Assignment(AS)
  )
where

import qualified Algebra.Graph.Undirected as U
import           Constraints              (Constraints ((\#\)))
import qualified Constraints              as C
import qualified Data.Set                 as S
import           Like                     (Like (like, (~~)))



infixl 0 ?

infixl 1 :?


data Cond a = a :? a

(?) :: Bool -> Cond a -> a
True ? (x :? _)  = x
False ? (_ :? y) = y

data Assignment i c
  = -- |  An Assingment has:
    --
    -- * id = **i**
    -- * constraints = **c**
    AS i c


instance (Eq i, Eq c) => Eq (Assignment i c) where
  (==) (AS i1 fc1) (AS i2 fc2) = i1 == i2

instance (Ord i, Eq i, Eq c) => Ord (Assignment i c) where
  (<=) (AS i1 fc1) (AS i2 fc2) = i1 <= i2

instance (Show i, Show c) => Show (Assignment i c) where
  show (AS a b) = "Assignment " <> show a <> "  " <> show b

instance (Like i) => Like (Assignment i c) where
  like (AS i1 _) (AS i2 _) = like i1 i2


c :: Assignment i c -> c
c (AS _ c ) = c

i :: Assignment i c -> i
i (AS i _ ) = i

-- | Takes Foldable of Assignments and retruns a list of Assignments\\
--   which have no colliding constraints
schedule :: (Ord i, Ord c, C.Constraints c, Foldable f, Like i) => f (Assignment i c) -> [Assignment i c]
schedule = schedule_ . constructGraph

-- | Takes Foldable of Assignments and retruns Foldable of Assignments\\
--   which have no colliding constraints
schedule_ :: (Ord i, Ord c, C.Constraints c, Like i) => U.Graph (Assignment i c) -> [Assignment i c]
schedule_  g
  | U.isEmpty g = []
  | otherwise = (\a -> a : schedule_  (selectedVertex a g)) . (\(a@(AS i c),lst) -> AS i $ C.minimize c (toCList lst) $ alikeLst a ) . tupleListMin $ U.adjacencyList g
    where toCList = map c
          alikeLst = \a -> toCList $ filter (~~a) $ U.vertexList g

-- |  Does not work on empty lists
--    NOTE: change to function that can handle empty lists
tupleListMin :: [(a,[b])] -> (a,[b])
tupleListMin [x] = x
tupleListMin (x:xs) = (length.snd) x > (length . snd ) o ? x :? o
  where o = tupleListMin xs

-- | Removes a Vertex and neighbouring conflicting constraints from the graph and vertecies.
selectedVertex :: (Ord c, Ord i, C.Constraints c) => Assignment i c -> U.Graph (Assignment i c) -> U.Graph (Assignment i c)
selectedVertex v@(AS iv ic) g = U.induce (\(AS i c)-> (not . C.null) c) . foldr (uncurry U.replaceVertex) (U.removeVertex v g) $ zip neighbours $ map (\(AS i c) -> AS i $ C.without c ic) neighbours
  where neighbours = S.toList $ U.neighbours v g



-- | constructs a graph where all the Vertecies contain Assignments \\
--   and are connected to the Assignments they have commone elements with
constructGraph :: (Ord i, Ord c, Foldable f, C.Constraints c) => f (Assignment i c) -> U.Graph (Assignment i c)
constructGraph = foldr insertVertex U.empty . foldr (\(AS i c) a -> AS i c : a) []

-- | inserts a Vertex into the graph connecting it with the Vertecies\\
--   which have intersecting elements
insertVertex :: (Ord c, Ord i, C.Constraints c) => Assignment i c -> U.Graph (Assignment i c) -> U.Graph (Assignment i c)
insertVertex a@(AS i c) g
  | U.isEmpty g = U.vertex a
  | otherwise = foldr (\e@(AS _ s) tmpG -> C.null (C.conflicts s c) ? free tmpG e :? bound tmpG e) g $ U.vertexList g
  where
    bound gt = U.overlay gt . U.connect (U.vertex a) . U.vertex
    free gt = U.overlay gt . U.overlay (U.vertex a) . U.vertex
