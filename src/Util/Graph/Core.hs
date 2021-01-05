{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Util.Graph.Core
Description : Graph data structure based on adjacency lists
Copyright   : (c) 2020 Nick Peterson
License     : GPL-3
Maintainer  : nick@nrp.dev
Stability   : experimental

This module contains a graph data structure (and helper methods) based on adjacency lists.
-}
module Util.Graph.Core (
  -- * Graph Data Structure
  --
  -- | Data structure representing graphs via adjacency lists implemented as @Map v [v]@.
  Graph(..),
  graphFromEdges,
  graphFromAdjs,

  -- * Lenses
  adjLists,

  -- * Vertex Properties
  -- 
  -- | Algorithms frequently require the ability to store one value per vertex. To keep code SOMEWHAT protected from
  -- implementation changes, we provide a type alias (`VertexProperty`) and a couple of convenience methods for 
  -- initializing such properties.
  VertexProperty,
  vertexProperty,
  initVertexProperty,

  -- * Helper Functions
  vertices,
  numEdges,
  edges,
  neighborsOf
) where

import Data.Map (Map, keys, fromList, fromListWith, assocs, mapWithKey)
import Data.List (sort, nub)
import Lens.Micro.Platform 

-- | Graph data structure based on maps from vertices to adjacency lists.
--
-- The graph is represented as a `Map`, keyed on vertices and with the list of adjacent vertices as values. Note that
-- this implies that vertex type should generally be an instance of `Ord`.
newtype Graph vert = Graph {
  _adjLists :: Map vert [vert] -- ^ Map from vertices to adjacency lists
} deriving Show
         
makeLenses ''Graph

-- | Data structure for storing properties per-vertex.
--
-- This type alias exists to provide simpler refactoring if the data structure needs to change.  Whatever the current
-- implementation, you should be able to access the value associated with @v@ by way of the `Traversal'` @ix v@.
type VertexProperty v a = Map v a

-- | Create a graph from a list of edges.
--
-- Note that the order of the vertices doesn't matter.  Also, the vertex set is implicitly defined by the edges --
-- the set of vertices is precisely the set of all vertices that appear in any edge.
graphFromEdges :: Ord v => [(v, v)]   -- ^ Collection of edges (as tuples where order doesn't matter) 
                           -> Graph v -- ^ Graph with the given edges (and vertex set derived from the edges)
graphFromEdges es = Graph $ sort . nub <$> adjMap
  where 
    adjMap = fromListWith (<>) pairs
    pairs = es >>= ( \ (v, w) -> [ (v, [w]), (w, [v]) ] )

-- | Create a graph from adjacency lists.
-- 
-- Note that the input adjacency lists are not checked in any way -- not for having all necessary entries, not for 
-- symmetry, and not for any other property other than type.          
graphFromAdjs :: Ord v => [(v, [v])] -> Graph v
graphFromAdjs = Graph . fromList . fmap ( _2 %~ sort )

-- | Retrieve a list of all vertices in a graph.
vertices :: Graph vert -> [vert]
vertices (Graph adj) = keys adj

-- | Compute the number of (undirected) edges in the input graph.
numEdges :: Graph vert -> Int
numEdges (Graph adjs) = sum numNeighbors `div` 2
  where numNeighbors = length <$> adjs

-- | Return a list of all edges in the graph.
edges :: Ord v => Graph v -> [(v, v)]
edges (Graph adjs) = assocs adjs >>= buildEdges
  where buildEdges (v, ns) = (v,) <$> filter (> v) ns

-- | Retrieve the list of vertices adjacent to a given vertex in a graph.
neighborsOf :: Ord v => v -> Graph v -> [v]
neighborsOf v (Graph adj) = adj ^?! ix v

-- | Helper for creating arrays indexed by the vertex set of a given graph.
--
-- Properties are initialized via a function that takes a graph and a vertex and produces a value.
vertexProperty :: Graph v -> (Graph v -> v -> a) -> VertexProperty v a
vertexProperty g f = mapWithKey mapper (g ^. adjLists)
  where mapper v _ = f g v

-- | Helper for creating arrays indexed by the vertex set of a given graph.
--
-- Properties are initialized to a constant value.
--
-- prop> initVertexProperty g a = vertexProperty g (\ _ _ -> a)
initVertexProperty :: Graph v -> a -> VertexProperty v a
initVertexProperty g a = vertexProperty g (\ _ _ -> a)