{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Util.Graph.ArticulationPoints
Description : Methods for computing articulation points for `Util.Graph.Core.Graph` instances
Copyright   : (c) 2020 Nick Peterson
License     : GPL-3
Maintainer  : nick@nrp.dev
Stability   : experimental

This module contains an implementation of Tarjan's Algorithm for finding articulation points of a graph. It is coded
using the generic DFS routine available in `Util.Graph.DFS`.
-}
module Util.Graph.ArticulationPoints (findArticulationPoints) where

import Control.Monad.State.Lazy
import Data.List (find, sort)
import Data.Maybe (isNothing, fromJust)
import Lens.Micro.Platform
import Util.Graph.Core
import Util.Graph.DFS

-- | Additional DFS state needed to support finding articulation points.
data ArtPointSt v = ArtPointSt {
    -- | The least vertex index which can be reached from this vertex or its subtree by way of back edges.
    --
    -- Note that a non-root vertex is an articulation point iff one of its tree children has @_apLow@ value at most the
    -- traversal number of this vertex.
    _apLow :: VertexProperty v Int,
    -- | Bools representation whether or not each vertex is known to be an articulation point
    _apIsAP :: VertexProperty v Bool,
    -- | If a vertex is an articulation point, its value in this array is a list of vertices constituting one connected
    -- component of the graph attained by deleting the vertex. (No guarantees are made about WHICH component.)
    _apExampleComponent :: VertexProperty v [v]
    }

makeLenses ''ArtPointSt

-- | Pre-traversal update function for articulation point state.
-- 
-- Initially assignes @apLow . ix v@ to the minimum of this vertex's visit number and the visit number of its back 
-- edges.
updateMinPre :: Ord v => v -> State (DFSState v (ArtPointSt v)) ()
updateMinPre v = do
  visitNums <- use stVisitOrder
  let vNum = fromJust $ visitNums ^?! ix v
  backNeighbors <- backEdges v <$> get
  let backNums = fromJust . (\ w -> visitNums ^?! ix w) <$> backNeighbors
  assign (stAppState . apLow . ix v)  $ minimum (vNum:backNums)

-- | Post-traversal update function for articulation point state.
--
-- When this fires for a given vertex, the @_apLow@ values for its children are already finalized; so, we can both
-- update this vertex's @_apLow@ value AND decide whether or not it is an articulation point.
updateMinPost :: Ord v => v -> State (DFSState v (ArtPointSt v)) ()
updateMinPost v = do
  curLows <- use (stAppState . apLow)
  visitNum <- fromJust . join <$> preuse (stVisitOrder . ix v)
  children <- knownChildren v <$> get
  parent <- fromJust <$> preuse (stParents . ix v)
  st <- get
  if isNothing parent 
  then when (length children > 1) $ do 
    assign (stAppState . apIsAP . ix v) True
    assign (stAppState . apExampleComponent . ix v) $ verticesRootedAt (last children) st
  else do 
    let descendentLows = fmap (\ w -> (w, curLows ^?! ix w)) children
    let compRoot = fst <$> find (\ (_, l) -> l >= visitNum ) descendentLows
    let comp = flip verticesRootedAt st <$> compRoot
    forM_ comp (\ c -> do assign (stAppState . apIsAP . ix v) True; assign (stAppState . apExampleComponent . ix v) c )
    let newMin = minimum $ (curLows ^?! ix v) : (snd <$> descendentLows)
    assign (stAppState . apLow . ix v) newMin

-- | Initial app-specific state for finding articulation points.
--
-- @_apLow@ is initialized to all @0@.
-- @_apIsAp@ is initialized to @False@.
-- @_apExampleComponent@ is initialized to empty list.
initAPState :: Graph v -> ArtPointSt v
initAPState g = ArtPointSt {
                             _apLow = initVertexProperty g 0,
                             _apIsAP = initVertexProperty g False,
                             _apExampleComponent = initVertexProperty g []
                           }

-- | Find the articulation points of the given graph -- that is, vertices whose removal disconnects the graph.
--
-- Uses Tarjan's DFS algorithm to find articulation points, and returns a list of pairs:
--     * The first coordinate of each represents an accumulation point
--     * The second is a list of vertices representing a connected component created by removing the accumulation point.
findArticulationPoints :: Ord v => Graph v -> [(v, [v])]
findArticulationPoints g = (\ v -> (v, comp ^?! ix v)) <$> filter (\ v -> isAP ^?! ix v) (vertices g)
  where 
    dfsResults = runDFS g (initAPState g) updateMinPre updateMinPost
    isAP = view (stAppState . apIsAP) dfsResults
    comp = sort <$> view (stAppState . apExampleComponent) dfsResults

