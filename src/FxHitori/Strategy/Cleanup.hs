{-# OPTIONS_HADDOCK hide #-}

module FxHitori.Strategy.Cleanup (cleanupBlack, cleanupWhite) where

import FxHitori.Board
import FxHitori.Strategy.Core
import Data.Map (singleton)
import Data.Maybe (isNothing, listToMaybe)
import Lens.Micro.Platform

handleWhite :: Board -> Cell -> [(Move, Explanation)]
handleWhite board cell = map buildMove targets
    where val = view cellValue cell
          targets = toListOf (rowColMateCells cell . filtered f) board
          f (Cell _ v st) = v == val && isNothing st
          buildMove (Cell p _ _) = (Move p Black, explanation)
          explanation = Explanation "Cleanup after white move" desc groups
          desc = "Cannot have two white cells with same value in row/col"
          groups = singleton "Known White Cell" [view cellPosition cell]

handleBlack :: Board -> Cell -> [(Move, Explanation)]
handleBlack board cell = map buildMove targets
    where targets = board ^.. adjCellsTo cell . filtered ( \ (Cell _ _ st) -> isNothing st)
          buildMove (Cell p _ _) = (Move p White, explanation)
          explanation = Explanation "Cleanup after black move" desc groups
          desc = "Cannot have two adjacent black cells"
          groups = singleton "Adjacent Black Cell" [view cellPosition cell]

cleanupWhiteMoves :: Board -> [(Move, Explanation)]
cleanupWhiteMoves board = do
    row <- view boardCells board
    cell <- row
    if view cellStatus cell == Just White then handleWhite board cell else []

cleanupBlackMoves :: Board -> [(Move, Explanation)]
cleanupBlackMoves board = do
    row <- view boardCells board
    cell <- row
    if view cellStatus cell == Just Black then handleBlack board cell else []

-- | Mark cells adjacent to `Black` cells as `White`.
cleanupBlack :: Strategy
cleanupBlack = Strategy (listToMaybe . cleanupBlackMoves)

-- | Mark cells in the same row/column as a `White` cell, and with the same value, as `Black`.
cleanupWhite :: Strategy
cleanupWhite = Strategy (listToMaybe . cleanupWhiteMoves)