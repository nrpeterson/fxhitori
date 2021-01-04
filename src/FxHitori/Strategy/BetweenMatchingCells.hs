{-# OPTIONS_HADDOCK hide #-}

module FxHitori.Strategy.BetweenMatchingCells (betweenMatchingCells) where

import FxHitori.Board
import Data.Map (singleton)
import Data.Maybe (listToMaybe)
import Data.List (transpose)
import FxHitori.Strategy.Core
import Util.List (zipWithMaybe3)

buildMoves :: Cell -> Cell -> Cell -> Maybe (Move, Explanation)
buildMoves (Cell p1 v1 _) (Cell p2 _ Nothing) (Cell p3 v3 _)
    | v1 == v3 = Just (mv, ex)
    | otherwise = Nothing
        where mv = Move p2 White
              sName = "Between Matching Cells"
              sDesc = "If this cell was black, these two neighbors with the same value would both be white " ++
                  "(which is not allowed)"
              groups = singleton "Adjacent Cells with Same Value" [p1, p3]
              ex = Explanation sName sDesc groups
buildMoves _ _ _ = Nothing

movesInRowOrCol :: [Cell] -> [(Move, Explanation)]
movesInRowOrCol cells = zipWithMaybe3 buildMoves cells (tail cells) (tail . tail $ cells)

moves :: Board -> [(Move, Explanation)]
moves (Board _ _ rows) = (rows ++ transpose rows) >>= movesInRowOrCol

-- | In triples of the form x y x, the middle cell (y) must be marked white.
betweenMatchingCells :: Strategy
betweenMatchingCells = Strategy (listToMaybe . moves)
