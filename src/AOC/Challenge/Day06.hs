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

inputParser :: Parser AsciiPoint
inputParser = curry AsciiPoint <$> integer <* symbol "," <*> integer

day06a :: [AsciiPoint] :~> Int
day06a =
  MkSol
    { sParse = parseManyEither inputParser
    , sShow = show
    , sSolve = Just . biggestRegion
    }

day06b :: [AsciiPoint] :~> Int
day06b =
  MkSol
    { sParse = parseManyEither inputParser
    , sShow = show
    , sSolve = Just . nearMost
    }

gridSize :: [AsciiPoint] -> (Int, Int)
gridSize = maximum . map apX &&& maximum . map apY

uniqueMinimumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
uniqueMinimumBy f = getOnly . head . groupBy (((EQ ==) .) . f) . sortBy f

biggestRegion :: [AsciiPoint] -> Int
biggestRegion is = snd grouped
  where
    (mx, my) = gridSize is
    coords = [AsciiPoint (x, y) | x <- [0 .. mx + 1], y <- [0 .. my + 1]]
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
    isEdge (AsciiPoint (x, y)) = x == 0 || y == 0 || x == mx + 1 || y == my + 1

nearMost :: [AsciiPoint] -> Int
nearMost is = S.size (expand initialSet initialSet)
  where
    xMid = listMiddle . sort . map apX $ is
    yMid = listMiddle . sort . map apY $ is
    initialSet :: S.Set AsciiPoint
    initialSet = S.fromList [AsciiPoint (x, y) | x <- xMid, y <- yMid]
    expand :: S.Set AsciiPoint -> S.Set AsciiPoint -> S.Set AsciiPoint
    expand inArea fringe
      | S.null new = inArea
      | otherwise = expand (S.union new inArea) new
      where
        candidates :: S.Set AsciiPoint
        candidates = S.unions $ S.map (S.fromList . neighbourhood) fringe
        new :: S.Set AsciiPoint
        new =
          S.filter ((< 10000) . totalDistance is) $
          S.difference candidates inArea

listMiddle :: Enum a => [a] -> [a]
listMiddle x =
  if even (l `mod` 2)
    then [Seq.index s (l `div` 2)]
    else [Seq.index s (l `div` 2) .. Seq.index s (1 + (l `div` 2))]
  where
    s = Seq.fromList x
    l = Seq.length s

totalDistance :: [AsciiPoint] -> AsciiPoint -> Int
totalDistance points c = sum $ map (manhattanDistance c) points
