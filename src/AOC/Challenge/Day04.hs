module AOC.Challenge.Day04
  ( day04a
  , day04b
  ) where

import           AOC.MinimalPrelude
import           Control.Applicative  ((<|>))
import           Control.Arrow        ((&&&))
import           Data.List            (group, maximumBy, sort)
import qualified Data.Map.Strict      as M
import           Data.Ord             (comparing)
import           Data.Time
import           Text.Megaparsec.Char (char)

data Action
  = GuardStart Int
  | FallsAsleep
  | Wakes
  deriving (Show, Eq, Ord)

type Line = (UTCTime, Action)

type Shift = (Int, [Int])

toShifts :: [Line] -> [Shift]
toShifts ls = map mkShift brokenUp
  where
    mins = todMin . timeToTimeOfDay . utctDayTime
    brokenUp = breakOn (isStart . snd) ls
    isStart (GuardStart _) = True
    isStart _              = False
    mkShift ((_, GuardStart i):rest) = (i, concatMap mkNap $ asPairs rest)
    mkShift _                        = error "No guardstart"
    mkNap ((t1, FallsAsleep), (t2, Wakes)) = [mins t1 .. (mins t2 - 1)]
    mkNap _                                = []

parseProb :: Parser Line
parseProb =
  (,) <$> (char '[' *> time <* symbol "]") <*> (guardStart <|> asleep <|> wakes)
  where
    guardStart =
      GuardStart <$> (symbol "Guard #" *> integer <* symbol "begins shift")
    asleep = const FallsAsleep <$> symbol "falls asleep"
    wakes = const Wakes <$> symbol "wakes up"

solvePartA :: [Shift] -> Int
solvePartA shifts = sleepiestGuard * minute
  where
    bg = M.fromListWith (++) shifts
    sleepiestGuard = fst . maximumBy (comparing (length . snd)) . M.toList $ bg
    gNaps = bg M.! sleepiestGuard
    minute = snd . maximum . map (length &&& head) . group . sort $ gNaps

day04a :: [Shift] :~> Int
day04a =
  MkSol
    { sParse = fmap (toShifts . sort) <$> parseManyMaybe parseProb
    , sShow = show
    , sSolve = Just . solvePartA
    }

solvePartB :: [Shift] -> Int
solvePartB shifts =
  uncurry (*) . head . maximumBy (comparing length) . group . sort $
  [(m, g) | (g, ns) <- shifts, m <- ns]

day04b :: [Shift] :~> Int
day04b =
  MkSol
    { sParse = fmap (toShifts . sort) <$> parseManyMaybe parseProb
    , sShow = show
    , sSolve = Just . solvePartB
    }
