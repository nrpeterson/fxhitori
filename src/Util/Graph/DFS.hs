{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Util.Graph.DFS
Description : Functions for running customized depth-first searches on `Util.Graph.Core.Graph` instances
Copyright   : (c) 2020 Nick Peterson
License     : GPL-3
Maintainer  : nick@nrp.dev
Stability   : experimental

This module contains functions for running depth-first searches on `Util.Graph.Core.Graph` instances,
allowing for app-specific state and update functions called after visiting each vertex (both pre- and post-recursion).  
It should be usable to implement a large number of different graph algorithms.
-}
module Util.Graph.DFS (
  -- * Module Re-Exports
  --
  -- | We re-export the contents of "Util.Graph.DFSState" to simplify imports.
  module Util.Graph.DFSState, 
  -- * Application-Specific DFS Runners
  --
  -- | These functions execute an application-specific depth-first search traversal. The specific application is
  -- represented by way of application-specific state on the `DFSState` object as well as an application-specific
  -- state update function (represented via @State (`DFSState` v appSt) ()@).
  runDFSFrom, 
  runDFS, 
  -- * Vanilla DFS Runners
  --
  -- | These functions execute a "vanilla" depth-first search traversal (that is, a DFS without application-specific
  -- state or updates).  These are equivalent to the application-specific DFS runners with @()@ for 
  -- application-specific state and @return ()@ for the application-specific state updater.
  runVanillaDFSFrom, 
  runVanillaDFS) where

import Control.Monad.State.Lazy
import Lens.Micro.Platform
import Util.Graph.Core
import Util.Graph.DFSState

dfsFrom :: Ord vert => vert -> Maybe vert -> State (DFSState vert appSt) ()
dfsFrom v maybeParent = do
  st <- get
  if isVisited v st then return ()
  else do
    visitOrder <- use stNextVisitNum
    assign (stVisitOrder . ix v) (Just visitOrder)
    modifying stNextVisitNum (+ 1)
    assign (stParents . ix v) maybeParent
    preUpdate <- view stPreRecurseUpdater <$> get
    _ <- preUpdate v
    g <- use stGraph
    mapM_ (\ w -> dfsFrom w (Just v)) (neighborsOf v g)
    postUpdate <- view stPostRecurseUpdater <$> get
    _ <- postUpdate v
    return ()

-- | Run an application-specific depth-first search from a specified vertex and return the final `DFSState`.
-- 
-- Note that this will ONLY run the traversal from the specified initial vertex -- vertices in other connected 
-- components will not be visited.
-- 
-- Specific applications of DFS are represented by two components:
--   * An app-specific state data type, with initial state provided as an argument
--   * Two app-specific state update functions (pre- and post-recursion), represented as 
--     @State (`DFSState` vert appSt) ()@.
-- 
-- Note that this update function is allowed to modify the entire `DFSState`, not just the app-specific state. As such,
-- this can be used to modify the behavior of the DFS or even change the graph itself before iterating to children.
runDFSFrom :: Ord v => v                               -- ^ Initial vex for DFS
                   -> Graph v                         -- ^ Graph
                   -> st                              -- ^ Initial application-specific state
                   -> (v -> State (DFSState v st) ()) -- ^ Application-specific pre-recurse state updater
                   -> (v -> State (DFSState v st) ()) -- ^ Application-specific post-recurse state updater
                   -> DFSState v st                   -- ^ Final `DFSState` after DFS is complete
runDFSFrom v g initAppSt preRecurse postRecurse = execState (dfsFrom v Nothing) st
    where st = initialState g initAppSt preRecurse postRecurse

dfs :: Ord vert => State (DFSState vert appSt) ()
dfs = do
    verts <- vertices <$> use stGraph
    mapM_ ( \ v -> dfsFrom v Nothing ) verts

-- | Run an application-specific depth-first search on a graph return the final `DFSState`.
-- 
-- This method differs from `runDFSFrom` in that it will create a depth-first traversal forest for disconnected graphs,
-- by choosing the next smallest vertex (in terms of the @Ord vert@ instance) when the current traversal is exhausted.
-- 
-- Specific applications of DFS are represented by two components:
--   * An app-specific state data type, with initial state provided as an argument
--   * Two app-specific state update functions (for pre- and post-recursion), represented as 
--     @State (`DFSState` vert appSt) ()@. 
-- 
-- Note that these update functions are allowed to modify the entire `DFSState`, not just the app-specific state. As 
-- such, this can be used to modify the behavior of the DFS or even change the graph itself before iterating to 
-- children.
runDFS :: Ord v => Graph v                         -- ^ Graph
               -> st                              -- ^ Initial application-specific state
               -> (v -> State (DFSState v st) ()) -- ^ Application-specific pre-recurse state updater
               -> (v -> State (DFSState v st) ()) -- ^ Application-specific post-recurse state updater
               -> DFSState v st                   -- ^ Final `DFSState` after DFS is complete
runDFS g initAppSt preRecurse postRecurse = execState dfs $ initialState g initAppSt preRecurse postRecurse

-- | Run "vanilla" depth-first search from a specified vertex and return the final `DFSState`.
--
-- Note that this will ONLY run the traversal from the specified initial vertex -- vertices in other connected
-- components will not be visited.
--
-- This only returns state common to all depth-first searches (parents and visit order).  It is equivalent to calling
-- `runDFSFrom` with unit @()@ app state and no-op state updaters.
runVanillaDFSFrom :: Ord vert => vert             -- ^ Initial vertex for DFS
                             -> Graph vert       -- ^ Graph
                             -> DFSState vert () -- ^ Final DFSState after DFS is complete
runVanillaDFSFrom v g = runDFSFrom v g () (const $ return ()) (const $ return ())

-- | Run "vanilla" depth-first search on a graph return the final `DFSState`.
--
-- This method differs from `runVanillaDFSFrom` in that it will create a depth-first traversal forest for disconnected
-- graphs, by choosing the next smallest vertex (in terms of the @Ord vert@ instance) when the current traversal is
-- exhausted.
--
-- This only returns state common to all depth-first searches (parents and visit order).  It is equivalent to calling
-- `runDFS` with unit @()@ app state and no-op state updaters.
runVanillaDFS :: Ord vert => Graph vert       -- ^ Graph
                         -> DFSState vert () -- ^ Final DFSState after DFS is complete
runVanillaDFS g = runDFS g () (const $ return ()) (const $ return ())