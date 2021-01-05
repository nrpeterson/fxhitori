{-# OPTIONS_HADDOCK hide #-}

module FxHitori.Strategy.Core (
  StratName,
  Description,
  Explanation(..),
  Strategy(..),
) where

import Control.Applicative
import Data.Map (Map)
import FxHitori.Board

-- | Name to report for a strategy.
--
-- Simply a string; this type alias is used for code readability.
type StratName = String

-- | Description of the logic behind a move.
--
-- Simply a string; this type alias is used for code readability.
type Description = String

-- | Explanation for a move.
--
-- Here, I've tried to optimize for two things:
--     1. The ability to show explanations in a UI. (Picture being able to highlight different groups of cells with
--        different colors, etc.)
--     2. The ability to characterize boards by how many times each strategy is invoked.
data Explanation = Explanation
                     StratName               -- ^ The name of the strategy used.
                     Description             -- ^ A description of the logic behind the move.
                     (Map String [Position]) -- ^ Named groups of cells relevant to the description.
                   deriving Show

-- | A strategy (heuristic) for playing Hitori.
--
-- This is essentially a function that given a board, might produce a `Move` and an `Explanation` for that move.
-- Note that this means strategies are currently stateless -- they will do a full computation on the board every single
-- time.  This is obviously inefficient and might be changed in the future.
newtype Strategy = Strategy {
  -- | The move function associated with this strategy.
  getMove :: Board -> Maybe (Move, Explanation)
}

-- | Strategies from a semigroup, where composition involves exhausting the left strategy before moving on to the right.
instance Semigroup Strategy where
  (Strategy f1) <> (Strategy f2) = Strategy (\ board -> f1 board <|> f2 board )

-- | The `Semigroup Strategy` instance actually forms a monoid. The identity strategy is one that always returns
-- `Nothing`.
instance Monoid Strategy where
    mempty = Strategy (const Nothing)