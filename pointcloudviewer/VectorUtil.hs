module VectorUtil
  ( kthSmallestBy
  , kthLargestBy
  ) where

import           Data.Ord (comparing, Down(..))
import           Data.Vector.Algorithms.Heap (partialSortBy)
import qualified Data.Vector.Generic as G


kthSmallestBy :: (G.Vector v a, Ord b) => (a -> b) -> Int -> v a -> a
kthSmallestBy f k vec
  | k < 1            = error "kLargestBy: k must be >= 1 if the vector is not empty"
  | k > G.length vec = error "kLargestBy: k must bet be > length of the vector"
  | otherwise        = G.modify (\mv -> partialSortBy (comparing f) mv k) vec G.! (k - 1)


kthLargestBy :: (G.Vector v a, Ord b) => (a -> b) -> Int -> v a -> a
kthLargestBy f = kthSmallestBy (Down . f)
