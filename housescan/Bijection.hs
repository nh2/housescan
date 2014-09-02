module Bijection
  ( biject
  ) where

import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.Set as Set


-- | Bijects a range of things into a continuous range of `Int`s.
--
-- The `Int`s are guaranteed to be assigned in the same order as they are given
-- in the input list (of course only the first occurrence matters).
--
-- Returns the two forward and backward bijeciton functions.
biject :: (Ord a) => [a] -> (a -> Int, Int -> a)
biject xs = (indexOf, aOfIndex)
  where
    uniqueAs = ordNub xs
    indexToAMap = IntMap.fromList $ zip [(0::Int)..] uniqueAs
    aToIndexMap = Map.fromList    $ zip uniqueAs     [0..]
    indexOf x  = let Just v = Map.lookup    x aToIndexMap in v
    aOfIndex x = let Just v = IntMap.lookup x indexToAMap in v


-- From https://github.com/nh2/haskell-ordnub
ordNub :: (Ord a) => [a] -> [a]
ordNub l = go Set.empty l
  where
    go _ [] = []
    go s (x:xs) = if x `Set.member` s then go s xs
                                      else x : go (Set.insert x s) xs
