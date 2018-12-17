module AOC.Challenge.Day01
  ( day01a
  , day01b
  ) where

import           AOC.MinimalPrelude
import           AOC.Solver         ((:~>) (..))

day01a :: [Int] :~> Int
day01a =
  MkSol
    { sParse = Just . map read . words . filter (/= '+')
    , sShow = show
    , sSolve = Just . sum
    }

day01b :: [Int] :~> Int
day01b =
  MkSol
    { sParse = Just . map read . words . filter (/= '+')
    , sShow = show
    , sSolve = findFirstDup . scanl (+) 0 . cycle
    }
