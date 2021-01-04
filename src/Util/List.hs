{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Util.List
Description : Helper methods for working with lists.
Copyright   : (c) 2020 Nick Peterson
License     : GPL-3
Maintainer  : nick@nrp.dev
Stability   : experimental

This module contains various helper functions related to lists.
-}

module Util.List (
  -- * Mappers / Zippers
  --
  -- | These are generalizations of the existing map / zip functions in @base@.
  mapWithIndex2d,
  zipWithMaybe,
  zipWithMaybe3,

  -- * Lenses / Traversals
  allExcept,
  rowColExcept,
  ixs
) where

import Lens.Micro.Platform
import Data.List (nub)

-- | Map over the given list-of-lists, giving the mapper function access to the index of the given element.
mapWithIndex2d :: (Int -> Int -> a -> b) -- ^ Mapper function, taking two indices (row, col) and the current value
               -> [[a]]                  -- ^ Current 2D list
               -> [[b]]                  -- ^ Resulting 2D list
mapWithIndex2d f = zipWith ( \ i -> zipWith (f i) [0..] ) [0..]

-- | Simultaneously zip and filter two lists.
--
-- Essentially, this is a combination of `zipWith` with `Data.Maybe.mapMaybe`. Zips the input lists and maps them with
-- the given function, filtering out values mapped to `Nothing` and keeping those mapped to `Just`.
zipWithMaybe :: (a -> b -> Maybe c) -- ^ Map/zip function. Map a pair of values to @Nothing@ or to a value.
             -> [a]                 -- ^ First input list
             -> [b]                 -- ^ Second input list
             -> [c]                 -- ^ Result list, throwing away `Nothing`s and unwrapping `Just`s.
zipWithMaybe f (x:xs) (y:ys) = let rest = zipWithMaybe f xs ys in
    case f x y of
        Just result -> result:rest
        Nothing -> rest
zipWithMaybe _ _ _ = []

-- | Simultaneously zip and filter three lists.
--
-- Essentially, this is a combination of `zipWith3` with `Data.Maybe.mapMaybe`. Zips the input lists and maps them with
-- the given function, filtering out values mapped to `Nothing` and keeping those mapped to `Just`.
zipWithMaybe3 :: (a -> b -> c -> Maybe d) -- ^ Map/zip function. Map a triple of values to @Nothing@ or to a value.
              -> [a]                      -- ^ First input list
              -> [b]                      -- ^ Second input list
              -> [c]                      -- ^ Third input list
              -> [d]                      -- ^ Result list, throwing away `Nothing`s and unwrapping `Just`s.
zipWithMaybe3 f (x:xs) (y:ys) (z:zs) = let rest = zipWithMaybe3 f xs ys zs in
    case f x y z of
        Just result -> result:rest
        Nothing -> rest
zipWithMaybe3 _ _ _ _ = []

-- | Traversal that covers all elements in the list EXCEPT the given index.
--
-- For example:
--
-- prop> [5,1,7,6] & allExcept 2 .~ 0 == [0,0,7,0]
allExcept :: Int -> Traversal' [a] a
allExcept 0 f (a:as) = (a:) <$> allExcept (-1) f as
allExcept i f (a:as) = (:) <$> f a <*> allExcept (i-1) f as
allExcept _ _ [] = pure []

-- | Traversal covering all elements of a given row and column in a 2d array OTHER than the element they share.
--
-- For example:
--
-- prop> [[1, 2, 3], [4, 5, 6], [7, 8, 9]] & rowColExcept (0, 1) .~ 0  == [[0, 2, 0], [4, 0, 6], [7, 0, 9]]
rowColExcept :: (Int, Int) -> Traversal' [[a]] a
rowColExcept (0, j) f (xs:xss) = (:) <$> allExcept j f xs <*> rowColExcept (-1, j) f xss
rowColExcept (i, j) f (xs:xss) = (:) <$> ix j f xs <*> rowColExcept (i-1, j) f xss
rowColExcept _ _ [] = pure []

-- | Generalization of `ix` that works with multiple distinct coordinates. Currently specialized for 2d lists.
--
-- For example:
--
-- prop> [[1, 2, 3], [4, 5, 6], [7, 8, 9]] & ixs [(0, 1), (1, 2)] .~ 0 == [[1, 0, 3], [4, 5, 0], [7, 8, 9]]
ixs :: [(Int, Int)] -> Traversal' [[a]] a
ixs is = ixs' (nub is)
    where ixs' [] _ xs = pure xs
          ixs' ((i, j):rest) f xs = case xs ^? (ix i . ix j) of
              Just xij -> set (ix i . ix j) <$> f xij <*> ixs rest f xs
              Nothing -> ixs rest f xs