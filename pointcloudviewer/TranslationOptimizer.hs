{-# LANGUAGE MultiWayIf #-}

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Packed.Matrix
import Data.Packed.Vector
import Numeric.LinearAlgebra.Algorithms (linearSolveLS)

import Debug.Trace


lstSqDistances :: Map (Int, Int) Double -> [Double]
lstSqDistances distMap = points
  where
    dists = Map.toList distMap
    n = succ . maximum $ concat [ [i,j] | ((i,j),_) <- dists ]

    a = fromColumns . drop 1 . toColumns $ -- drop first col as we want x_0 = 0
          fromRows [ fromList $ mkRow (i,j) | ((i,j),_) <- dists ]

    -- Puts a -1 at position i and a +1 at position j
    mkRow (i,j) = [ (if | p == i    -> -1
                        | p == j    ->  1
                        | otherwise ->  0) | p <- [0..n-1] ]


    b = asColumn $ fromList [ d | (_, d) <- dists ]

    [x] = toColumns $ linearSolveLS a b
    points = traceShow a $ toList x
