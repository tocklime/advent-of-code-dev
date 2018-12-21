module AOC.Challenge.Day18
  ( day18a
  , day18b
  , drawGrid
  ) where

import           AOC.MinimalPrelude
import           Control.Applicative  ((<|>))
import           Data.Array
import           Data.Bool            (bool)
import           Data.Functor         (($>))
import qualified Data.Map.Strict      as M
import           Text.Megaparsec      (eof, many)
import           Text.Megaparsec.Char (char)

day18a :: Grid :~> Int
day18a =
  MkSol
    { sParse = parseEither parseGrid
    , sShow = show
    , sSolve = Just . scoreWorld . (!! 10) . iterate stepGrid
    }

day18b :: Grid :~> Int
day18b =
  MkSol
    { sParse = parseEither parseGrid
    , sShow = show
    , sSolve = Just . stepB . runUntilLoop
    }

data Cell
  = Open
  | Tree
  | Lumberyard
  deriving (Eq, Ord)

instance Show Cell where
  show = (: []) . drawCell

stepB :: (Grid, Int, Int) -> Int
stepB (g, firstOccurence, secondOccurence) =
  scoreWorld $ iterate stepGrid g !! nSteps
  where
    loopSize = secondOccurence - firstOccurence
    nSteps = (1000000000 - secondOccurence) `mod` loopSize

runUntilLoop :: Grid -> (Grid, Int, Int)
runUntilLoop = go M.empty 0
  where
    go seenMap n g =
      case M.lookup g seenMap of
        Just n' -> (g, n', n)
        Nothing -> go (M.insert g n seenMap) (n + 1) (stepGrid g)

scoreWorld :: Grid -> Int
scoreWorld g =
  (length . filter (== Tree) $ es) * (length . filter (== Lumberyard) $ es)
  where
    es = elems g

drawCell :: Cell -> Char
drawCell Open       = '.'
drawCell Tree       = '|'
drawCell Lumberyard = '#'

drawGrid :: Grid -> String
drawGrid g =
  unlines [[drawCell (g ! (x, y)) | x <- [minX .. maxX]] | y <- [minY .. maxY]]
  where
    ((minX, minY), (maxX, maxY)) = bounds g

parseCell :: Parser Cell
parseCell =
  (char '.' $> Open) <|> (char '|' $> Tree) <|> (char '#' $> Lumberyard)

type Neighbourhood = [Cell]

type Grid = Array (Int, Int) Cell

hasN :: Cell -> Int -> Neighbourhood -> Bool
hasN c i = (>= i) . length . filter (== c)

-- hasN Tree 3 [Open, Tree, Lumber, Tree, Lumber]
step :: Cell -> [Cell] -> Cell
step Open = bool Open Tree . hasN Tree 3
step Tree = bool Tree Lumberyard . hasN Lumberyard 3
step Lumberyard =
  bool Open Lumberyard . (\n -> hasN Tree 1 n && hasN Lumberyard 1 n)

around :: (Int, Int) -> Grid -> Neighbourhood
around (x, y) g =
  [ g ! (x', y')
  | x' <- [(max minX (x - 1)) .. (min maxX (x + 1))]
  , y' <- [(max minY (y - 1)) .. (min maxY (y + 1))]
  , (x, y) /= (x', y')
  ]
  where
    ((minX, minY), (maxX, maxY)) = bounds g

stepGrid :: Grid -> Grid
stepGrid g = array (bounds g) [(i, step e (around i g)) | (i, e) <- assocs g]

parseGrid :: Parser Grid
parseGrid = do
  ls <- Text.Megaparsec.many cellLine
  eof
  return $
    array
      ((0, 0), (length (head ls) - 1, length ls - 1))
      [((x, y), e) | (y, l) <- zip [0 ..] ls, (x, e) <- zip [0 ..] l]
  where
    cellLine = Text.Megaparsec.many parseCell <* char '\n'
