{-# LANGUAGE MultiWayIf #-}

module TranslationOptimizer
  ( lstSqDistances
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Packed.Matrix
import Data.Packed.Vector
import Numeric.Container
import qualified Numeric.Container as Mat
import Numeric.LinearAlgebra.Algorithms (linearSolveLS)

import Bijection (biject)


type RMSE = Double


lstSqDistances :: (Ord a) => Map (a, a) Double -> (Map a Double, RMSE)
lstSqDistances distMap = (res, rmse)
  where
    -- `biject` takes care of duplicates for us.
    (indexOf, aOfIndex) = biject $ concat [ [a,b] | (a,b) <- Map.keys distMap ]

    (pos, rmse) = lstSqDistancesI (Map.mapKeys (\(a,b) -> (indexOf a, indexOf b)) distMap)
    res = Map.fromList $ zip (map aOfIndex [0..]) pos


-- TODO Add allowing hard (in)equality constraints, e.g. to forbid
--      the least-squares optimization pushing two rooms into each other.

lstSqDistancesI :: Map (Int, Int) Double -> ([Double], RMSE)
lstSqDistancesI distMap = (points, rmse)
  where
    dists = Map.toList distMap
    n = succ . maximum $ concat [ [i,j] | ((i,j),_) <- dists ]

    a = fromColumns . drop 1 . toColumns $ -- drop first col as we want x_0 = 0
          fromRows [ fromList $ mkRow (i,j) | ((i,j),_) <- dists ]

    -- Puts a -1 at position i and a +1 at position j
    mkRow (i,j) = [ (if | p == i    -> -1
                        | p == j    ->  1
                        | otherwise ->  0) | p <- [0..n-1] ]


    b = fromList [ d | (_, d) <- dists ]

    [x] = toColumns $ linearSolveLS a (asColumn b) -- TODO Figure out how to catch `ghc: linearSolverLSR: singular`
    points = 0.0 : toList x -- prepend a 0 for x_0

    rmse = sqrt (norm2 ( (a Mat.<> x) `Mat.sub` b ) / fromIntegral (dim b))
