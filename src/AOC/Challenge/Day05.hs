module AOC.Challenge.Day05
  ( day05a
  , day05b
  ) where

import           AOC.MinimalPrelude
import           Data.Char          (toLower)

day05a :: String :~> Int
day05a = MkSol {sParse = Just, sShow = show, sSolve = Just . length . minimise}

day05b :: String :~> Int
day05b =
  MkSol
    { sParse = Just
    , sShow = show
    , sSolve =
        Just . minimum . (\t -> map (minLenWithout t) ['a' .. 'z']) . minimise
    }

match :: Char -> Char -> Bool
match a b = toLower a == toLower b && a /= b

minimise :: String -> String
minimise = go []
  where
    go ls [] = ls
    go (l:ls) (r:rs)
      | match l r = go ls rs
      | otherwise = go (r : l : ls) rs
    go [] (r:rs) = go [r] rs

minLenWithout :: String -> Char -> Int
minLenWithout t c = length . minimise . filter ((/= c) . toLower) $ t
