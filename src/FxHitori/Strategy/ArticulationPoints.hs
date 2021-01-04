{-# OPTIONS_HADDOCK hide #-}

module FxHitori.Strategy.ArticulationPoints (
  articulationPoints
) where

import Data.Map (singleton)
import Data.Maybe (isNothing, listToMaybe)
import FxHitori.Board
import FxHitori.Strategy.Core
import Util.Graph.ArticulationPoints

basicMoveFunc :: Board -> [(Move, Explanation)]
basicMoveFunc board = buildMove <$> filter ( \ (pos, _) -> isNothing (statusAt pos board)) artPoints
    where g = toNeighborGraph board
          artPoints = findArticulationPoints g
          buildMove (pos, comp) = (mv, ex)
              where mv = Move pos White
                    sName = "Articulation Points"
                    sDesc = "Making the cell at " ++ show pos ++ " black would disconnect the board"
                    groups = singleton "Disconnected Cells" comp
                    ex = Explanation sName sDesc groups

-- | If marking a cell `Black` would cause the board to become disconnected, then the cell must be `White`.
--
-- Note that this is "dumb" -- it will not identify articulation points unless all other involved cells in blocking the
-- path are already marked `Black`.  So, it will not find articulation points if part of the path is blocked by, say, a
-- pair of cells with the same number where one or the other must be black.
articulationPoints :: Strategy
articulationPoints = Strategy (listToMaybe . basicMoveFunc)

