{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day07
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 7.  See "AOC.Solver" for the types used in this module!
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
module AOC.Challenge.Day07
  ( day07a
  , day07b
  ) where

import           AOC.MinimalPrelude
import           Control.Arrow        ((&&&))
import           Data.Char            (ord)
import           Data.List            (foldl', partition)
import qualified Data.Map             as M
import qualified Data.Set             as S
import           Text.Megaparsec.Char (letterChar)

day07a :: M.Map Char (S.Set Char) :~> String
day07a =
  MkSol
    { sParse = fmap mkPrereqs <$> parseManyEither lineParser
    , sShow = id
    , sSolve = Just . solvePuzzle
    }

day07b :: M.Map Char (S.Set Char) :~> Int
day07b =
  MkSol
    { sParse = fmap mkPrereqs <$> parseManyEither lineParser
    , sShow = show
    , sSolve = Just . scheduleWork
    }

-- day7a :: Problem
-- day7a = problemSimple "7A" "7" $ solvePuzzle . mkPrereqs . unsafeParse (many lineParser)
-- day7b :: Problem
-- day7b = problemSimple "7B" "7" $ scheduleWork . mkPrereqs . unsafeParse (many lineParser)
type Prereq = (Char, S.Set Char)

lineParser :: Parser Prereq
lineParser = do
  a <- symbol "Step " *> letterChar <* symbol " must be finished before step"
  b <- letterChar <* symbol " can begin."
  return (b, S.singleton a)

mkPrereqs :: [Prereq] -> M.Map Char (S.Set Char)
mkPrereqs ls = M.fromListWith S.union $ ls ++ allMentioned
  where
    allMentioned = map (, S.empty) . S.toList . S.unions . map snd $ ls

solvePuzzle :: M.Map Char (S.Set Char) -> String
solvePuzzle m
  | M.null m = []
  | otherwise =
    case removeFirstEmpty m of
      Just (x, m') -> x : solvePuzzle m'
      Nothing      -> error "Circular dependencies"

markDone :: M.Map Char (S.Set Char) -> Char -> M.Map Char (S.Set Char)
markDone m c = M.map (S.delete c) . M.delete c $ m

markManyDone :: String -> M.Map Char (S.Set Char) -> M.Map Char (S.Set Char)
markManyDone cs m = foldl' markDone m cs

removeFirstEmpty ::
     M.Map Char (S.Set Char) -> Maybe (Char, M.Map Char (S.Set Char))
removeFirstEmpty m =
  case mFirstEmpty of
    Nothing     -> Nothing
    Just (c, _) -> Just (c, markDone m c)
  where
    mFirstEmpty = M.lookupMin $ M.filter S.null m

allDoable :: M.Map Char (S.Set Char) -> String
allDoable = M.keys . M.filter S.null

duration :: Char -> Int
duration c = 61 + (ord c - ord 'A')

workers :: Int
workers = 5

scheduleWork :: M.Map Char (S.Set Char) -> Int
scheduleWork = go 0 []
  where
    go t currentWork m
      | null work = t
      | otherwise = go (t + 1) work newMap
      where
        newMap = markManyDone (map fst finished) m
        (finished, ongoing) = partition ((<= t) . snd) currentWork
        availableWorkers = workers - length ongoing
        ongoingChars = map fst ongoing
        newWork =
          map (id &&& (+ t) . duration) .
          take availableWorkers . filter (`notElem` ongoingChars) $
          allDoable newMap
        work = ongoing ++ newWork
