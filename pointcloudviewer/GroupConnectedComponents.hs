module GroupConnectedComponents
  ( groupConnectedComponents
  ) where

import           Data.Graph (Edge, buildG)
import qualified Data.Graph as Graph
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.Tree as Tree

import           Bijection (biject)


-- | Given a list of graph edges with attached data, partitions it into the
-- undirected connected components.
groupConnectedComponents :: (Ord node) => [ ((node, node), a) ] -> [[ ((node, node), a) ]]
groupConnectedComponents edgesData = res
  where
    -- Biject the nodes into a contiguous integer range (since Data.Graph uses an array).
    (bij, unbij) = biject $ concat [ [i,j] | ((i,j),_) <- edgesData ]

    bijEdgesData = [ ((bij i, bij j), a) | ((i,j), a) <- edgesData ]

    -- Allows us to recover the data attached to edges, from the bijected edges.
    bijEdgeDataMap = Map.fromList bijEdgesData
    getData edge = let Just a = Map.lookup edge bijEdgeDataMap in a

    bijComps = groupCCContiguous (map fst bijEdgesData)

    -- Unbiject edges to original values and re-attach their data to them.
    res = [ [ ( (unbij i, unbij j), getData (i,j) ) | (i,j) <- comp ]
          | comp <- bijComps ]


-- | Given a list of graph edges, partitions it into the undirected connected
-- components.
--
-- PRE: Vertices in edges must be in a contiguous range (not checked).
groupCCContiguous  :: [Edge] -> [ [Edge] ]
groupCCContiguous [] = []
groupCCContiguous edges = IntMap.elems compToEdges
  where
    bounds = let allVertices = concat [ [a,b] | (a,b) <- edges ]
              in (minimum allVertices, maximum allVertices) -- not partial because edges /= []

    graph = buildG bounds edges
    comps = map Tree.flatten (Graph.components graph)

    -- Tells for each vertex in which component (index) it is.
    vertToComp = IntMap.fromList [ (v, i) | (i, c) <- zip [(0::Int)..] comps, v <- c ]

    -- Tells for component component (index) which edges belong to it.
    -- `lookup` is total because each vertex belongs to a component.
    compToEdges = IntMap.fromListWith (++) [ (c, [(i, j)]) | (i,j) <- edges, let Just c = IntMap.lookup i vertToComp ]
