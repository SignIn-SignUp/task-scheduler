{-# LANGUAGE ExistentialQuantification #-}

module Scheduler.Assignment
  ( resolve,
    Assignment (AS),
  )
where

import qualified Algebra.Graph.Undirected as U
import Data.Foldable (Foldable(toList))
import qualified Data.Set as S (toList)
import Scheduler.Constraints (Constraints((\#\)))
import qualified Scheduler.Constraints as C
import Scheduler.Like (Like(like, (~~)))

infixl 0 ?

infixl 1 :?

data Cond a = a :? a

(?) :: Bool -> Cond a -> a
True ? (x :? _) = x
False ? (_ :? y) = y

-- |  An Assingment has:
--
-- * id = **i**
-- * constraints = **c**
data Assignment i c
  = -- |  AS is the only constructor of Assignment:
    --
    -- * id = **i**
    -- * constraints = **c**
    AS i c

instance (Eq i, Eq c) => Eq (Assignment i c) where
  (==) (AS i1 fc1) (AS i2 fc2) = i1 == i2 && fc1 == fc2

instance (Ord i, Ord c, Eq i, Eq c) => Ord (Assignment i c) where
  (<=) (AS i1 fc1) (AS i2 fc2)
    | i1 < i2 = True
    | i1 == i2 && fc1 <= fc2 = True
    | otherwise = False

instance (Show i, Show c) => Show (Assignment i c) where
  show (AS a b) = "Assignment " <> show a <> "  " <> show b

instance (Eq i) => Like (Assignment i c) where
  like (AS i1 _) (AS i2 _) = i1 == i2

c :: Assignment i c -> c
c (AS _ c) = c

i :: Assignment i c -> i
i (AS i _) = i

-- |  Takes Foldable of Assignments and retruns a list of Assignments
--    which have no colliding constraints. \\
--    'minimize' from 'Constraints' is used to select the Constraints
--    of the respective Assignment. \\
--    It is given in that order:
--
--    * the Constraints to minimize
--    * the colliding Constraints
--    * the "alike" Constraints.\\
--      The ones of which the respective Assignments a and b satisfy (like a b)\\
--      with 'like' from 'Like'
resolve :: (Ord i, Ord c, C.Constraints c, Foldable f, Like i) => f (Assignment i c) -> [Assignment i c]
resolve = resolve_ . constructGraph . toList

-- |  Takes Undirectional Graph of Assignments and retruns list of Assignments\\
--    which have no colliding constraints.
resolve_ :: (Ord i, Ord c, C.Constraints c, Like i) => U.Graph (Assignment i c) -> [Assignment i c]
resolve_ g
  | U.isEmpty g = []
  | otherwise = (\(mimzd,nbrs,gwa) -> mimzd : resolve_ (replaceWithConstraints (c mimzd) nbrs gwa)) . minimizeAndRemoveVertex g . tupleListMin $ U.adjacencyList g
  where
    toCList = map c
    alikeLst = \a -> toCList $ filter (~~ a) $ U.vertexList g
    minimizeAndRemoveVertex = \gr (a@(AS i c), lst) -> (AS i $ C.minimize c (toCList lst) $ alikeLst a,lst,U.removeVertex a gr)

-- |  Returns the minimum of a tuplelist where the min is \\
--    the element with the least elements in the list of snd. \\
--    &#x26A0; Does not work on empty lists. \\
--      **NOTE:** change to function that can handle empty lists
tupleListMin :: [(a, [b])] -> (a, [b])
tupleListMin [x] = x
tupleListMin (x : xs) = (length . snd) x > (length . snd) o ? x :? o
  where
    o = tupleListMin xs


-- |  Removes a constraints from the vetecies and then replaces the vercecies in graph.
replaceWithConstraints :: (Ord c, Ord i, C.Constraints c) => c -> [Assignment i c] -> U.Graph (Assignment i c) -> U.Graph (Assignment i c)
replaceWithConstraints ic neighbours g = replaceVercecies g $ zip neighbours $ removeContraints ic neighbours

-- |  Removes the conflicting contraints from each element in the list. \\
--    Same as applying without with given Constraints to all constraints of the list.
removeContraints :: Constraints c => c -> [Assignment i c] -> [Assignment i c]
removeContraints ic = map (\(AS i c) -> AS i $ C.without c ic)

-- |  Replaces x with y from [(x,y)] in the given graph.
replaceVercecies :: (Ord c, Ord i, C.Constraints c) => U.Graph (Assignment i c) -> [(Assignment i c, Assignment i c)] -> U.Graph (Assignment i c)
replaceVercecies g a = U.induce (\(AS i c) -> (not . C.null) c) $ foldr (uncurry U.replaceVertex) g a


-- |  Constructs a graph where all the Vertecies contain Assignments and \\
--    are connected to the Assignments they have common constraints with. \\
--    &#x24D8; Assignments with (null constraints) will not be inserted.
constructGraph :: (Ord i, Ord c, Foldable f, C.Constraints c) => f (Assignment i c) -> U.Graph (Assignment i c)
constructGraph = foldr (\v g -> (C.null.c) v  ? g :? insertVertex v g) U.empty

-- |  Inserts a Vertex into the graph connecting it with the Vertecies \\
--    which have intersecting elements.
insertVertex :: (Ord c, Ord i, C.Constraints c) => Assignment i c -> U.Graph (Assignment i c) -> U.Graph (Assignment i c)
insertVertex a@(AS i c) g
  | U.isEmpty g = U.vertex a
  | otherwise = foldr (\e@(AS _ s) tmpG -> C.null (C.conflicts s c) ? free tmpG e :? bound tmpG e) g $ U.vertexList g
  where
    bound gt = U.overlay gt . U.connect (U.vertex a) . U.vertex
    free gt = U.overlay gt . U.overlay (U.vertex a) . U.vertex
