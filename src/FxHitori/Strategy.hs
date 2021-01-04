{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : FxHitori.Strategy
Description : Representation of Hitori strategy, and pre-defined strategies
Copyright   : (c) 2020 Nick Peterson
License     : GPL-3
Maintainer  : nick@nrp.dev
Stability   : experimental

This module contains data structures and helper functions for building and using strategies for solving Hitori games.
-}

module FxHitori.Strategy (
  -- * Basic Data Types and Type Aliases
  -- ** Explanations for Moves
  StratName,
  Description,
  Explanation(..),
  -- ** Strategy
  Strategy(..),
  -- * Executing Strategies
  stepRunner,
  -- * Specific Strategies
  cleanupBlack,
  cleanupWhite,
  adjacentPairs,
  betweenMatchingCells,
  articulationPoints,
  -- * Useful Strategy Combinations
  fullSolver
) where

import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy
import Control.Monad.Trans.Maybe
import FxHitori.Board
import FxHitori.Strategy.AdjacentPairs
import FxHitori.Strategy.ArticulationPoints
import FxHitori.Strategy.BetweenMatchingCells
import FxHitori.Strategy.Cleanup
import FxHitori.Strategy.Core

-- | Lift a `Strategy` into a stateful monadic computation.
--
-- Uses MTL typeclasses to enforce two properties of the given monad:
--    1. It must have a `Board` as state
--    2. It must be a writer, collecting @[(`Move`, `Explanation`)]@.
--
-- The resulting computation runs the state through the `Strategy` to (try to) produce a move and explanation, then:
--    1. Updates the `Board` state to reflect that move being taken
--    2. Records that move and explanation to the log
--
-- Note that the result is wrapped in `MaybeT`, which allows us to run to completion (exhaustion of the strategy)
-- by simply asking it to repeat this action forever; as soon as the `Strategy` produces a `Nothing`, computation will
-- stop. It can of course be unwrapped into a @m (`Maybe` (`Move`, `Explanation`))@ by way of `runMaybeT`.
stepRunner :: (MonadState Board m, MonadWriter [(Move, Explanation)] m) => Strategy -> MaybeT m (Move, Explanation)
stepRunner strat = do
  (mv, ex) <- MaybeT $ getMove strat <$> get
  modify (applyMove mv)
  tell [(mv, ex)]
  return (mv, ex)

-- | Combination of all strategies, in a "sensible" order of increasing difficulty.
--
-- At the moment, this strategy is able to solve most easy puzzles, but gets stuck fairly quick on medium or harder.
--
-- Currently, this consists of:
--
--     * `cleanupBlack`
--     * `cleanupWhite`
--     * `adjacentPairs`
--     * `betweenMatchingCells`
--     * `articulationPoints`
fullSolver :: Strategy
fullSolver = mconcat [cleanupBlack, cleanupWhite, adjacentPairs, betweenMatchingCells, articulationPoints]