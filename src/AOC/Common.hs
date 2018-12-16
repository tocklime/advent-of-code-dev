-- |
-- Module      : AOC.Challenge
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Meant to be a place to include common functionality used across
-- different parts in the challenge.
--


module AOC.Common (
  findFirstDup
  ) where

import qualified Data.Set as S

findFirstDup :: (Ord a) => [a] -> Maybe a
findFirstDup = go S.empty
  where
    go _ [] = Nothing
    go seen (x:xs) 
     | x `S.member` seen = Just x 
     | otherwise = go (S.insert x seen) xs
