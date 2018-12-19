module AOC.Challenge.Day06
  ( day06a
  , day06b
  ) where

import           AOC.MinimalPrelude
import           Control.Arrow      ((&&&))
import           Data.Function      (on)
import           Data.List          (groupBy, maximumBy, nub, sort, sortBy,
                                     sortOn)
import           Data.Maybe         (isJust)
import           Data.Ord           (comparing)
import qualified Data.Sequence      as Seq
import qualified Data.Set           as S

type Coord = (Int, Int)

type Input = [Coord]

inputParser :: Parser Coord
inputParser = (,) <$> integer <* symbol "," <*> integer

day06a :: [Coord] :~> Int
day06a =
  MkSol
    { sParse = parseManyEither inputParser
    , sShow = show
    , sSolve = Just . biggestRegion
    }

day06b :: [Coord] :~> Int
day06b =
  MkSol
    { sParse = parseManyEither inputParser
    , sShow = show
    , sSolve = Just . nearMost
    }

gridSize :: Input -> (Int, Int)
gridSize is = (x, y)
  where
    x = maximum . map fst $ is
    y = maximum . map snd $ is

uniqueMinimumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
uniqueMinimumBy f = getOnly . head . groupBy (((EQ ==) .) . f) . sortBy f

manhattanDistance :: Coord -> Coord -> Int
manhattanDistance (a, b) (c, d) = abs (c - a) + abs (d - b)

biggestRegion :: [Coord] -> Int
biggestRegion is = snd grouped
  where
    (mx, my) = gridSize is
    coords = [(x, y) | x <- [0 .. mx + 1], y <- [0 .. my + 1]]
    maybeClosest c = uniqueMinimumBy (comparing (manhattanDistance c)) is
    dists =
      [ (x, c)
      | c <- coords
      , let maybex = maybeClosest c
      , isJust maybex
      , let Just x = maybex
      ]
    edges = filter (isEdge . snd) dists
    infinites = nub . sort . map fst $ edges
    grouped =
      maximumBy (comparing snd) .
      filter ((`notElem` infinites) . fst) .
      map (fst . head &&& length) . groupBy ((==) `on` fst) . sortOn fst $
      dists
    isEdge (x, y) = x == 0 || y == 0 || x == mx + 1 || y == my + 1

nearMost :: [Coord] -> Int
nearMost is = S.size (expand initialSet initialSet)
  where
    xMid = listMiddle . sort . map fst $ is
    yMid = listMiddle . sort . map snd $ is
    initialSet :: S.Set Coord
    initialSet = S.fromList [(x, y) | x <- xMid, y <- yMid]
    expand :: S.Set Coord -> S.Set Coord -> S.Set Coord
    expand inArea fringe
      | S.null new = inArea
      | otherwise = expand (S.union new inArea) new
      where
        candidates :: S.Set Coord
        candidates = S.unions $ S.toList $ S.map neighbours fringe
        new :: S.Set Coord
        new =
          S.filter ((< 10000) . totalDistance is) $
          S.difference candidates inArea
        neighbours :: Coord -> S.Set Coord
        neighbours (x, y) =
          S.fromList [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

listMiddle :: Enum a => [a] -> [a]
listMiddle x =
  if even (l `mod` 2)
    then [Seq.index s (l `div` 2)]
    else [Seq.index s (l `div` 2) .. Seq.index s (1 + (l `div` 2))]
  where
    s = Seq.fromList x
    l = Seq.length s

totalDistance :: [Coord] -> Coord -> Int
totalDistance points c = sum $ map (manhattanDistance c) points
