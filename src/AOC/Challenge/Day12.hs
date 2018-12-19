module AOC.Challenge.Day12
  ( day12a
  , day12b
  ) where

import           AOC.MinimalPrelude
import qualified Data.Map.Strict      as M
import           Data.Maybe           (fromMaybe)
import qualified Data.Set             as S
import           Text.Megaparsec      (eof, many, oneOf)
import           Text.Megaparsec.Char ()

day12a :: PlantPots :~> Integer
day12a =
  MkSol
    { sParse = parseEither problemParse
    , sShow = show
    , sSolve = Just . score . nstep 20
    }

day12b :: PlantPots :~> Integer
day12b =
  MkSol
    { sParse = parseEither problemParse
    , sShow = show
    , sSolve = return . solvePart2 . last . part2
    }

score :: PlantPots -> Integer
score = fromIntegral . sum . S.toList . ppState

solvePart2 :: (Integer, Integer, Integer) -> Integer
solvePart2 (steps, scoreSoFar, diffPerStep) =
  scoreSoFar + diffPerStep * (50000000000 - fromIntegral steps)

part2 :: PlantPots -> [(Integer, Integer, Integer)]
part2 = go 0 0 0
  where
    go ::
         Integer
      -> Integer
      -> Integer
      -> PlantPots
      -> [(Integer, Integer, Integer)]
    go n nIdenticalDiffs lastDiff p
      | nIdenticalDiffs > 10 = []
      | otherwise = (n, sp, thisDiff) : rest
      where
        p' = step p
        sp = score p
        sp' = score p'
        thisDiff = sp' - sp
        rest =
          go
            (n + 1)
            (if thisDiff == lastDiff
               then nIdenticalDiffs + 1
               else 0)
            thisDiff
            p'

data PlantPots = PlantPots
  { ppState :: S.Set Int
  , _rules  :: [Bool] -> Bool
  }

nstep :: Integer -> PlantPots -> PlantPots
nstep 0 !p  = p
nstep !n !p = nstep (n - 1) (step p)

step :: PlantPots -> PlantPots
step (PlantPots !s !r) = PlantPots newState r
  where
    smallestSet = S.findMin s
    largestSet = S.findMax s
    newState = S.fromList . filter isSet $ [smallestSet - 2 .. largestSet + 2]
    isSet n = r $ map (`S.member` s) [n - 2 .. n + 2]

hashesParse :: Parser [Bool]
hashesParse = do
  s <- Text.Megaparsec.many $ oneOf "#."
  let initial = (== '#') <$> s
  sc
  return initial

ruleParse :: Parser ((Bool, Bool, Bool, Bool, Bool), Bool)
ruleParse = do
  [a, b, c, d, e] <- hashesParse
  symbol "=>"
  [r] <- hashesParse
  return ((a, b, c, d, e), r)

problemParse :: Parser PlantPots
problemParse = do
  symbol "initial state: "
  initial <- S.fromList . map fst . filter snd . zip [0 ..] <$> hashesParse
  rs <- Text.Megaparsec.many ruleParse
  let m = M.fromList rs
  let f l =
        case l of
          [a, b, c, d, e] -> fromMaybe False $ M.lookup (a, b, c, d, e) m
          _               -> False
  eof
  return $ PlantPots initial f
