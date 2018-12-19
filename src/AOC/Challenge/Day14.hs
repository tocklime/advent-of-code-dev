module AOC.Challenge.Day14
  ( day14a
  , day14b
  ) where

import           AOC.MinimalPrelude
import           Data.List          (isPrefixOf, tails)
import qualified Data.Sequence      as S
import           Text.Read          (readMaybe)

day14a :: Int :~> [Int]
day14a =
  MkSol
    { sParse = maybeToEither "Not a number" . readMaybe
    , sShow = concatMap show
    , sSolve = Just . solve14
    }

day14b :: String :~> Int
day14b = MkSol {sParse = return . strip, sShow = show, sSolve = Just . solve14b}

digits :: Int -> [Int]
digits n
  | n < 10 = [n]
  | otherwise = [1, n `mod` 10]

-- largely cribbed from https://github.com/mstksg/advent-of-code-2018/blob/master/reflections.md#day-14 , because my own initial solution had an off-by-one error.
elfSequence :: [Int]
elfSequence = 3 : 7 : go 0 1 (S.fromList [3, 7])
  where
    go !p1 !p2 !board = new ++ go p1' p2' board'
      where
        aVal = S.index board p1
        bVal = S.index board p2
        new = digits $ aVal + bVal
        board' = board <> S.fromList new
        p1' = (p1 + aVal + 1) `mod` S.length board'
        p2' = (p2 + bVal + 1) `mod` S.length board'

solve14 :: Int -> [Int]
solve14 n = take 10 $ drop n elfSequence

subStrLoc :: Eq a => [a] -> [a] -> Int
subStrLoc xs = length . takeWhile (not . (xs `isPrefixOf`)) . tails

solve14b :: String -> Int
solve14b digs = s `subStrLoc` elfSequence
  where
    s = map (read . (: [])) digs
