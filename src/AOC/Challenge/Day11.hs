{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day11
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 11.  See "AOC.Solver" for the types used in this module!
--
-- After completing the challenge, it is recommended to:
--
-- *   Replace "AOC.Prelude" imports to specific modules (with explicit
--     imports) for readability.
-- *   Remove the @-Wno-unused-imports@ and @-Wno-unused-top-binds@
--     pragmas.
-- *   Replace the partial type signatures underscores in the solution
--     types @_ :~> _@ with the actual types of inputs and outputs of the
--     solution.  You can delete the type signatures completely and GHC
--     will recommend what should go in place of the underscores.
module AOC.Challenge.Day11
  ( day11a
  , day11b
  ) where

import           AOC.Prelude
import qualified Data.Array    as A
import           Data.Array.ST (newArray, readArray, runSTArray, writeArray)

day11a :: Int :~> _
day11a =
  MkSol
    { sParse = readMaybe
    , sShow = \(_, (b, c)) -> show b <> "," <> show c
    , sSolve = Just . best . sumsOfSize 3 . preGrid
    }

day11b :: Int :~> _
day11b =
  MkSol
    { sParse = readMaybe
    , sShow = \(_, (a, b, c)) -> intercalate "," . map show $ [a, b, c]
    , sSolve = Just . best . allSums . preGrid
    }

onlyHundreds :: Int -> Int
onlyHundreds x = (x `mod` 1000) `div` 100

powerLevel :: Int -> (Int, Int) -> Int
powerLevel serial (x, y) =
  subtract 5 . onlyHundreds . (* (x + 10)) . (+ serial) . (* y) . (+ 10) $ x

preGrid :: Int -> Grid
preGrid serial =
  runSTArray $ do
    arr <- newArray ((1, 1), (301, 301)) 0
    forM_ [(x, y) | x <- [1 .. 301], y <- [1 .. 301]] $ \(x, y) -> do
      up <-
        if y > 1
          then readArray arr (x, y - 1)
          else pure 0
      left <-
        if x > 1
          then readArray arr (x - 1, y)
          else pure 0
      upleft <-
        if x > 1 && y > 1
          then readArray arr (x - 1, y - 1)
          else pure 0
      writeArray arr (x, y) (powerLevel serial (x, y) + up + left - upleft)
    return arr

type Grid = A.Array (Int, Int) Int

neighbourhoodSum :: Grid -> Int -> (Int, Int) -> Int
neighbourhoodSum grid s (a, b) = upleft + bottomright - left - up
  where
    upleft =
      if a > 1 && b > 1
        then grid A.! (a - 1, b - 1)
        else 0
    bottomright = grid A.! (a + s - 1, b + s - 1)
    left =
      if a > 1
        then grid A.! (a - 1, b + s - 1)
        else 0
    up =
      if b > 1
        then grid A.! (a + s - 1, b - 1)
        else 0

best :: Ord a => [(a, b)] -> (a, b)
best = maximumBy (comparing fst)

sumsOfSize :: Int -> Grid -> [(Int, (Int, Int))]
sumsOfSize n !grid =
  [ (neighbourhoodSum grid n (x, y), (x, y))
  | x <- [1 .. 300 - n]
  , y <- [1 .. 300 - n]
  ]

allSums :: Grid -> [(Int, (Int, Int, Int))]
allSums !grid =
  [ (b, (x, y, s))
  | s <- [299,298 .. 1]
  , let (b, (x, y)) = best $ sumsOfSize s grid
  ]
