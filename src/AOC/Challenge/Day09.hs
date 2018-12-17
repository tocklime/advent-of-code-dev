module AOC.Challenge.Day09
  ( day09a
  , day09b
  ) where

import           AOC.MinimalPrelude
import           Control.Arrow      (second)
import qualified Data.Map           as M
import           Data.Sequence      (ViewL ((:<)), (<|))
import qualified Data.Sequence      as S

day09a :: (Int, Int) :~> Int
day09a =
  MkSol
    { sParse = parseMaybe parseProb
    , sShow = show
    , sSolve = Just . winner . marbles
    }

day09b :: (Int, Int) :~> Int
day09b =
  MkSol
    { sParse = parseMaybe parseProb
    , sShow = show
    , sSolve = Just . winner . marbles . second (* 100)
    }

parseProb :: Parser (Int, Int)
parseProb = do
  p <- integer <* symbol "players; last marble is worth"
  points <- integer <* symbol "points"
  return (p, points)

splitAtCycle :: Int -> S.Seq a -> (S.Seq a, S.Seq a)
splitAtCycle n s = S.splitAt (n `mod` S.length s) s

winner :: M.Map Int Int -> Int
winner = maximum . M.elems

marbles :: (Int, Int) -> M.Map Int Int
marbles (nPlayers, turnLimit) =
  play (S.singleton 0) 0 (M.fromList [(n, 0) | n <- [0 .. nPlayers - 1]])
  where
    play board currentTurn scores
      | currMarble > turnLimit = scores
      | currMarble `mod` 23 == 0 = play takenBoard (currentTurn + 1) newScores
      | otherwise = play newBoard (currentTurn + 1) scores
      where
        currMarble = currentTurn + 1
        currPlayer = currentTurn `mod` nPlayers
        (l, r) = splitAtCycle 2 board
        newBoard = (currentTurn + 1) <| r <> l
        (sl, sr) = splitAtCycle (S.length board - 7) board
        (t :< sr') = S.viewl sr
        takenBoard = sr' <> sl
        newScores = M.adjust ((+ t) . (+ currMarble)) currPlayer scores
