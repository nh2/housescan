{-# LANGUAGE MultiWayIf #-}

module TranslationOptimizer
  ( lstSqDistances
  ) where

import Control.Applicative
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Packed.Matrix
import Data.Packed.Vector
import Numeric.Container
import qualified Numeric.Container as Mat

import Bijection (biject)
import HmatrixUtils (safeLinearSolveLS)


type RMSE = Double


-- | Least-squares fit a position constraint graph in one dimension.
--
-- Each "edge" ((i,j), d) in the given map expresses that the desired distance
-- between nodes `i` and `j` be `d`. Negative `d` are allowed.
--
-- This function then returns the optimal position for each node.
-- the first node of the first edge given is placed at 0, and all other nodes
-- are placed at their respective signed offsets from that node.
--
-- The `Map` as input makes sure that only 1 distiance is given for every
-- requirement "edge". That is not per se necessary for lstSqDistances to work
-- (since the idea is that it can solve inconsistent systems), but for many
-- problems, the inconsitencies arise transitively, and this API makes sure
-- that direct contradictions are not passed.
lstSqDistances :: (Ord a) => Map (a, a) Double -> Maybe (Map a Double, RMSE)
lstSqDistances distMap = do -- Maybe monad
    (pos, rmse) <- lstSqDistancesI (Map.mapKeys (\(a,b) -> (indexOf a, indexOf b)) distMap)
    return (Map.fromList $ zip (map aOfIndex [0..]) pos, rmse)
  where
    -- `biject` takes care of duplicates for us.
    (indexOf, aOfIndex) = biject $ concat [ [a,b] | (a,b) <- Map.keys distMap ]


-- TODO Add allowing hard (in)equality constraints, e.g. to forbid
--      the least-squares optimization pushing two rooms into each other.

lstSqDistancesI :: Map (Int, Int) Double -> Maybe ([Double], RMSE)
lstSqDistancesI distMap =
  let dists = Map.toList distMap
      n = succ . maximum $ concat [ [i,j] | ((i,j),_) <- dists ]

      a = fromColumns . drop 1 . toColumns $ -- drop first col as we want x_0 = 0
            fromRows [ fromList $ mkRow (i,j) | ((i,j),_) <- dists ]

      -- Puts a -1 at position i and a +1 at position j
      mkRow (i,j) = [ (if | p == i    -> -1
                          | p == j    ->  1
                          | otherwise ->  0) | p <- [0..n-1] ]


      b = fromList [ d | (_, d) <- dists ]

   in do -- Maybe monad

        [x] <- toColumns <$> safeLinearSolveLS a (asColumn b)

        let points = 0.0 : toList x -- prepend a 0 for x_0 (we dropped it from the system input above)

            rmse = sqrt (norm2 ( (a Mat.<> x) `Mat.sub` b ) / fromIntegral (dim b))

        return (points, rmse)
