{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Util.Graph.DFSState
Description : Representation of state for application-specific depth-first search via "Util.Graph.DFS"
Copyright   : (c) 2020 Nick Peterson
License     : GPL-3
Maintainer  : nick@nrp.dev
Stability   : experimental

This module contains a representation of state for application-specific depth-first searches, for use with the
"Util.Graph.DFS" module.  It also contains some convenience functions for working with such state, to make DFS cleaner.
-}
module Util.Graph.DFSState (
  -- * DFS State Representation
  DFSState(..),
  initialState,
  -- * Lenses
  stParents,
  stVisitOrder,
  stNextVisitNum,
  stAppState,
  stPreRecurseUpdater,
  stPostRecurseUpdater,
  stGraph,
  -- * Helper Methods
  isVisited,
  backEdges,
  knownChildren,
  verticesRootedAt
) where

import Control.Monad.State.Lazy
import Data.Maybe (isJust, fromJust)
import Lens.Micro.Platform
import Util.Graph.Core

-- | State for a depth-first search application.
--
-- This includes:
--   * Standard DFS state (parents, visit order, next visit number)
--   * The graph itself
--   * Application-specific state, in the form of a data object and pre/post-recursion state updaters
--
-- Note that the state updaters are allowed to modify ALL state, not just the application-specific
-- state; this means in practice that a DFS application can actually modify the behavior of the DFS (or even modify the
-- graph itself) on the fly.
data DFSState v st = DFSState { -- | Known vertex parents in the DFS tree / forest
                                _stParents :: VertexProperty v (Maybe v),
                                -- | Visit order of vertices in the DFS, where known
                                _stVisitOrder :: VertexProperty v (Maybe Int),
                                -- | Visit order number to assign to the next visited vertex
                                _stNextVisitNum :: Int,
                                -- | Application-specific state
                                _stAppState :: st,
                                -- | Application-specific pre-recursion state updater function.
                                --
                                -- Note that this can modify ALL state, not just the application-specific state.
                                _stPreRecurseUpdater :: v -> State (DFSState v st) (),
                                -- | Application-specific post-recursion state updater function.
                                -- 
                                -- Note that this can modify ALL state, not just the application-specific state.
                                _stPostRecurseUpdater :: v -> State (DFSState v st) (),
                                -- | The graph on which the traversal is operating
                                _stGraph :: Graph v
                              }

makeLenses ''DFSState

-- | Create a `DFSState` instance representing the initialization of a depth-first search.
--
-- Takes a graph, application-specific initial state, and the application-specific state updater. The arrays for
-- parents and traversal order are initialized with @Nothing@s, and next visit number is set to 0.
initialState :: Ord v => Graph v                         -- ^ Graph to be traversed
                      -> st                              -- ^ Initial application-specific state
                      -> (v -> State (DFSState v st) ()) -- ^ Application-specific pre-recurse state update function
                      -> (v -> State (DFSState v st) ()) -- ^ Application-specific post-recurse state update function
                      -> DFSState v st                   -- ^ Initial state for a depth-first search
initialState g initAppSt preUpdate postUpdate = DFSState { _stParents = initVertexProperty g Nothing,
                                                           _stVisitOrder = initVertexProperty g Nothing,
                                                           _stNextVisitNum = 0,
                                                           _stAppState = initAppSt,
                                                           _stPreRecurseUpdater = preUpdate,
                                                           _stPostRecurseUpdater = postUpdate,
                                                           _stGraph = g }

-- | Check whether the given vertex has been visited within the current traversal (represented by `DFSState`).
isVisited :: Ord v => v                -- ^ Vertex to check
                   -> DFSState v appSt -- ^ State of the traversal
                   -> Bool             -- ^ True if the vertex has been visited, and False otherwise
isVisited v st = isJust visitOrder
  where visitOrder = st ^?! (stVisitOrder . ix v)

-- | Find all back-edges for the given vertex, relative to the DFS tree (perhaps partial) stored in the `DFSState`.
--
-- The @_stParents@ property holds known parents, with @Nothing@ representing roots or vertices not yet explored.
-- Back edges are defined as edges from a given vertex whose other end has a lower traversal number (and is not @v@'s 
-- immediate parent).
backEdges :: Ord v => v             -- ^ The vertex whose edges you want 
                   -> DFSState v st -- ^ The current state of a DFS traversal
                   -> [v]           -- ^ Vertices reachable from @v@ via back-edges
backEdges v dfsState = filter ( \ w -> parent /= Just w && visitedBeforeV w) $ neighborsOf v g
    where 
      g = dfsState ^. stGraph
      vNums = view stVisitOrder dfsState
      vOrd = fromJust $ vNums ^?! ix v
      parent = dfsState ^?! stParents . ix v
      visitedBeforeV w = case vNums ^?! ix w of
        Just wOrd -> wOrd < vOrd
        Nothing   -> False

-- | Find direct children (known so far) of @v@ in the (perhaps partial) DFS tree represented by `DFSState`.
knownChildren :: Ord v => v             -- ^ The vertex whose known children you want
                       -> DFSState v st -- ^ The current state of a DFS traversal
                       -> [v]           -- ^ Known children of @v@ in the current DFS so far
knownChildren v dfsState = filter vIsParent $ neighborsOf v g
  where 
    g = view stGraph dfsState
    vIsParent w = parents ^?! ix w == Just v
    parents = view stParents dfsState 

-- | Find all (known so far) descendants of @v@ in the (perhaps partial) DFS tree represented by `DFSState`.   
verticesRootedAt :: Ord v => v             -- ^ The vertex whose descendants you want
                          -> DFSState v st -- ^ The current state of a DFS traversal
                          -> [v]           -- ^ Known descendants of @v@ in the current DFS so far
verticesRootedAt v dfsState = v : subDescendants
  where subDescendants = knownChildren v dfsState >>= flip verticesRootedAt dfsState