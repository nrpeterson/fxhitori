{-# OPTIONS_HADDOCK hide #-}

module FxHitori.Strategy.AdjacentPairs (adjacentPairs) where

import Data.List (transpose)
import Data.Map (singleton)
import Data.Maybe (mapMaybe, isNothing, listToMaybe)
import FxHitori.Board
import FxHitori.Strategy.Core
import Util.List (zipWithMaybe)

buildMove :: (Int, Position, Position) -> Cell -> Maybe (Move, Explanation)
buildMove (v, p1, p2) (Cell pc vc st)
    | vc == v && pc /= p1 && pc /= p2 && isNothing st = Just (mv, ex)
    | otherwise = Nothing
        where mv = Move pc Black
              stratName = "Adjacent Pairs"
              desc = "When two cells with the same value are adjacent, one must be white; so, all other cells in " ++
                  "that row/column must be black"
              groups = singleton "Adjacent Cells with Same Value" [p1, p2]
              ex = Explanation stratName desc groups

adjPairMovesInRowOrCol :: [Cell] -> [(Move, Explanation)]
adjPairMovesInRowOrCol cells = targets >>= findMoves
    where targets = zipWithMaybe getTargets cells (tail cells)
          getTargets (Cell pos1 val1 Nothing) (Cell pos2 val2 Nothing) =
              if val1 == val2 then Just (val1, pos1, pos2) else Nothing
          getTargets _ _ = Nothing
          findMoves target = mapMaybe (buildMove target) cells

adjPairMoves :: Board -> [(Move, Explanation)]
adjPairMoves (Board _ _ rows) = (rows ++ transpose rows) >>= adjPairMovesInRowOrCol

-- | If two adjacent cells have the same number, all other cells in their shared row/column must be `Black`.
adjacentPairs :: Strategy
adjacentPairs = Strategy (listToMaybe . adjPairMoves)