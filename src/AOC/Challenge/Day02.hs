module AOC.Challenge.Day02
  ( day02a
  , day02b
  ) where

import           AOC.Common
import           AOC.Solver ((:~>) (..))
import           Data.Bool  (bool)
import           Data.List  (group, sort)

data Line =
  Line Int
       Int

instance Semigroup Line where
  (Line a b) <> (Line c d) = Line (a + c) (b + d)

instance Monoid Line where
  mempty = Line 0 0

lineProduct :: Line -> Int
lineProduct (Line a b) = a * b

analyse :: String -> Line
analyse = (\x -> Line (has 2 x) (has 3 x)) . map length . group . sort
  where
    has n x = bool 0 1 (n `elem` x)

day02a :: [String] :~> Int
day02a =
  MkSol
    { sParse = Just . lines
    , sShow = show
    , sSolve = Just . lineProduct . mconcat . map analyse
    }

day02b :: [String] :~> String
day02b =
  MkSol
    { sParse = Just . lines
    , sShow = id
    , sSolve = fmap (filter (/= '_')) . findFirstDup . concatMap expand
    }

expand :: String -> [String]
expand t = map (blankNth t) [0 .. length t - 1]

blankNth :: String -> Int -> String
blankNth t n =
  let (a, b) = splitAt n t
   in a <> "_" <> tail b
