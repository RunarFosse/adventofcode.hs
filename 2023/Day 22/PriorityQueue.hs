module PriorityQueue where

-- Haskell PriorityQueue implementation
-- https://adinapoli.github.io/alfredodinapoli.com/posts/2017-04-07-the-simplest-possible-haskell-heap-implementation.html

import Data.List hiding (singleton, insert)

type Rank = Int

data Heap a = Tip | Node {-# UNPACK #-} !Rank a (Heap a) (Heap a) deriving Show

-- Rank: the length of the path between the node and the right most leaf.
rank Tip = 0
rank (Node r _ _ _) = r

fromList :: Ord a => [a] -> Heap a
fromList [] = Tip
fromList (x:xs) = foldl' (\hp val -> insert val hp) (singleton x) xs

makeHeap :: a -> Heap a -> Heap a -> Heap a
makeHeap x a b = if rank a >= rank b then Node (rank b + 1) x a b
                                     else Node (rank a + 1) x b a

empty :: Heap a
empty = Tip

singleton :: a -> Heap a
singleton x = Node 1 x Tip Tip

insert :: Ord a => a -> Heap a -> Heap a
insert x h = merge (singleton x) h

-- | Merge two heaps together, preserving the leftist property via `makeHeap`.
-- Runs in O(log n).
-- "The key insight behind leftist heaps is that two heaps can be merged by
-- merging their right spines as you would merge two sorted lists, and then
-- swapping the children of nodes along this path as necessary to restore
-- the leftist property." -- Okasaki
merge :: Ord a => Heap a -> Heap a -> Heap a
merge l Tip = l
merge Tip r = r
merge h1@(Node _ x l1 r1) h2@(Node _ y l2 r2) =
  if x <= y then makeHeap x l1 (merge r1 h2)
            else makeHeap y l2 (merge h1 r2)

-- | O(1).
peekMin :: Heap a -> Maybe a
peekMin Tip = Nothing
peekMin (Node _ x _ _) = Just x

-- | O(1), but evaluating the second element of the tuple has same complexity
-- of `merge`.
extractMin :: Ord a => Heap a -> Maybe (a, Heap a)
extractMin Tip = Nothing
extractMin (Node _ x a b) = Just (x, merge a b)