{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : FxHitori.Board
Description : Core data structures and functions for representing a game of Hitori
Copyright   : (c) 2020 Nick Peterson
License     : GPL-3
Maintainer  : nick@nrp.dev
Stability   : experimental

This module contains data structures and helper functions for orchestrating a game of Hitori.
-}

module FxHitori.Board (
  -- * Basics
  Position,
  Status(..),

  -- * Cells
  Cell(..),

  -- ** Lenses
  cellPosition,
  cellValue,
  cellStatus,

  -- * Moves
  Move(..),

  -- ** Lenses
  movePosition,
  moveType,

  -- * Board
  Board(..),

  -- ** Basic Lenses
  boardNumRows,
  boardNumCols,
  boardCells,

  -- ** Additional Optics
  cellAt,
  rowColMateCells,
  adjCellsTo,

  -- ** Helper Methods
  statusAt,
  toNeighborGraph,
  toSharedValueGraph,

  -- ** IO Helpers
  boardFromNums,
  printBoard,

  -- * Game Mechanics
  applyMove
) where

import Data.List (intercalate)
import Data.Maybe (isNothing)
import Lens.Micro.Platform
import Util.Graph.Core
import Util.List

-- | Position of a cell on the board.  Type aliased for readability.
type Position = (Int, Int)

-- | The different (known) types that cells can have and moves can represent.
--
-- (Of course, the status of a `Cell` may be unknown, so they are represented as @Maybe `Status`@.)
data Status = White -- ^ White \/ circled \/ clear -- a space over which travel is allowed
            | Black -- ^ Black \/ X'd \/ blocked -- a space over which travel is not allowed
            deriving (Show, Eq, Enum)

-- | A cell on the game board.
--
-- Cells have a position, an integer value, and a status (which might be unknown, or might be `Black` or `White`).
data Cell = Cell { _cellPosition :: Position,  -- ^ Position of the cell on the board (0-indexed in each coordinate)
                   _cellValue :: Int,          -- ^ Number that appears on the cell
                   _cellStatus :: Maybe Status -- ^ Status of cell, if known
                 } deriving (Show, Eq)

makeLenses ''Cell

-- | A move represents the intention to change the state of a cell on the board to either `White` or `Black`.
data Move = Move { _movePosition :: Position, -- ^ The position of the `Cell` to be changed
                   _moveType :: Status        -- ^ What the new status of that `Cell` should be
                 } deriving (Show, Eq)

makeLenses ''Move

-- | The actual Hitori game board.
--
-- The board consists of a grid of `Cell`s, where each `Cell` has a number and a state (unknown, marked
-- `White`, or marked `Black`)
data Board = Board { _boardNumRows :: Int,   -- ^ Number of rows in the game board
                     _boardNumCols :: Int,   -- ^ Number of columns in the game board
                     _boardCells :: [[Cell]] -- ^ The `Cell`s of the board, represented as nested lists
                   }

makeLenses ''Board

-- | A `Traversal'` that traverses the cell at a specific position of the board (if it exists).
cellAt :: Position -> Traversal' Board Cell
cellAt (i, j) = boardCells . ix i . ix j

-- | A `Traversal'` that traverses ALL cells of the game board.
allCells :: Traversal' Board Cell
allCells = boardCells . each . each

-- | A `Traversal'` that traverses all cells whose status has not yet been determined.
unmarkedCells :: Traversal' Board Cell
unmarkedCells = allCells . filtered (\ (Cell _ _ st) -> isNothing st)

-- | A `Traversal'` that traverses all cells whose status is either `White` or unknown.
nonBlackCells :: Traversal' Board Cell
nonBlackCells = allCells . filtered (\ (Cell _ _ st) -> st /= Just Black)

-- | Apply a a `Move` to a `Board`, creating a new `Board` with a cell's status updated.
applyMove :: Move -> Board -> Board
applyMove (Move pos st) = cellAt pos . cellStatus ?~ st

-- | A `Traversal'` that traverses all cells in the same row and column as the given position, but not the position
-- itself.
--
-- This is useful for considering the impact of marking a cell `White`, since we must make any other cell of the same
-- value in its row/column `Black` as a result.
rowColMateCells :: Cell -> Traversal' Board Cell
rowColMateCells (Cell pos _ _) = boardCells . rowColExcept pos

-- | A `Traversal'` that traverses the cells adjacent to the given position (but not the position itself).
--
-- This is useful for considering the impact of marking a cell `Black`, since all adjacent cells must be marked
-- `White`.
adjCellsTo :: Cell -> Traversal' Board Cell
adjCellsTo (Cell (i, j) _ _) = boardCells . ixs [(i-1, j), (i, j-1), (i, j+1), (i+1, j)]

-- | Fetch the status of the cell at the given position.
statusAt :: Position -> Board -> Maybe Status
statusAt pos = ( ^?! (cellAt pos . cellStatus))

-- | Generate a blank hitori board (all statuses `Nothing`) with the given cell values.
--
-- Note that this method does not check that all rows are of the same length.
boardFromNums :: [[Int]] -> Board
boardFromNums vals = Board nRows nCols cells
    where cells = mapWithIndex2d cellBuilder vals
          cellBuilder i j val = Cell (i, j) val Nothing
          nRows = length vals
          nCols = length $ head vals

-- | Create a pretty-printed view of the given Hitori board.
--
-- Cells are displayed as their integer value. Explicitly marked cells use a character to represent their status (
-- using @x@ for `Black` and @o@ for `White`).  Unmarked cells have no character.
--
-- @
-- ---------------------------------------------------
-- | 2x | 8o | 2x | 6o | 10 | 1  | 9  | 4  | 2o | 4  |
-- ---------------------------------------------------
-- | 10o| 9o | 8o | 4  | 4  | 7  | 1o | 5  | 5  | 6  |
-- ---------------------------------------------------
-- | 2  | 3  | 5  | 1  | 7  | 2  | 9  | 10o| 4  | 5  |
-- ---------------------------------------------------
-- | 4  | 7  | 9o | 7  | 8  | 5  | 3o | 5  | 1  | 8  |
-- ---------------------------------------------------
-- | 7  | 5  | 6  | 10 | 4  | 8o | 4  | 9  | 6  | 1  |
-- ---------------------------------------------------
-- | 1o | 3  | 5  | 9  | 6o | 4  | 2o | 8  | 10 | 5  |
-- ---------------------------------------------------
-- | 7  | 2  | 4  | 8o | 10x| 10o| 10x| 1o | 5  | 3  |
-- ---------------------------------------------------
-- | 9  | 6  | 3  | 2  | 1o | 3  | 5o | 8x | 8o | 2  |
-- ---------------------------------------------------
-- | 4  | 10 | 1o | 7  | 3  | 4  | 7  | 6o | 1x | 9o |
-- ---------------------------------------------------
-- | 5  | 10 | 3  | 9  | 10 | 6o | 10 | 8  | 7o | 2  |
-- ---------------------------------------------------
-- @
printBoard :: Board -> String
printBoard (Board _ nCols cells) = line ++ intercalate line tableRows ++ line
    where line = "\n" ++ replicate (5 * nCols + 1) '-' ++ "\n"
          tableRows = map buildRow cells
          buildRow row = "|" ++ intercalate "|" (map buildCell row) ++ "|"
          buildCell (Cell _ val st) = pad (show val ++ stateMark st)
          stateMark Nothing = ""
          stateMark (Just Black) = "x"
          stateMark (Just White) = "o"
          pad s
              | length s >= 4 = take 4 s
              | length s == 3 = " " ++ s
              | length s == 2 = " " ++ s ++ " "
              | length s == 1 = " " ++ s ++ "  "
              | otherwise = "   "

-- | Build a graph from the given board, where vertices are positions and edges connect adjacent positions.
--
-- Cells explicitly marked `Black` are not included in the graph.
toNeighborGraph :: Board -> Graph (Int, Int)
toNeighborGraph board = graphFromAdjs adjs
    where cells = board ^.. nonBlackCells
          adjs = buildAdjs <$> cells
          buildAdjs cell = (cell ^. cellPosition, (^. cellPosition) <$> nbrs)
              where nbrs = board ^.. adjCellsTo cell . filtered notBlack
          notBlack (Cell _ _ st) = st /= Just Black

-- | Biuld a graph from the given board, where vertices are positions and edges connect cells that share a row/column
-- and have the same value.
--
-- Cells explicitly marked `Black` are not included in the graph.
toSharedValueGraph :: Board -> Graph (Int, Int)
toSharedValueGraph board = graphFromAdjs adjs
    where cells = board ^.. nonBlackCells
          adjs = buildAdjs <$> cells
          buildAdjs cell = (pos, (^. cellPosition) <$> board ^.. matches)
              where (Cell pos val _) = cell
                    matches = rowColMateCells cell . filtered (\ (Cell _ v st) -> isNothing st && v == val)




