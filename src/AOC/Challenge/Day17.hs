{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day17
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 17.  See "AOC.Solver" for the types used in this module!
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
module AOC.Challenge.Day17
  ( day17a
  , day17b
  ) where

import           AOC.Prelude
import           Control.Monad.Reader
import           Data.Bool            (bool)
import qualified Data.Map.Strict      as M
import           Text.Megaparsec
import           Text.Megaparsec.Char

day17a :: _ :~> _
day17a =
  MkSol
    { sParse = fmap (initWorld . concat) <$> parseManyEither parseLine
    , sShow = show
    , sSolve = Just . countCells (/= Clay) . fillWorld
    }

day17b :: _ :~> _
day17b =
  MkSol
    { sParse = fmap (initWorld . concat) <$> parseManyEither parseLine
    , sShow = show
    , sSolve = Just . countCells (== SettledWater) . fillWorld
    }

data Cell
  = Clay
  | SettledWater
  | RunningWater
  deriving (Eq)

type World = M.Map AsciiPoint Cell

type WorldStep = StateT World (Reader Int)

initWorld :: [AsciiPoint] -> World
initWorld = M.fromList . fmap (, Clay)

runWorld :: World -> WorldStep a -> (a, World)
runWorld w = flip runReader maxY . flip runStateT w
  where
    maxY = maximum . map apY . M.keys $ w

countCells :: (Cell -> Bool) -> World -> Int
countCells p w = M.size . M.filterWithKey (\k a -> p a && apY k >= minY) $ w
  where
    minY = minimum . map apY . M.keys . M.filter (== Clay) $ w

fillWorld :: World -> World
fillWorld w = snd $ runWorld w (down (AsciiPoint (500, 0)))

drawCell :: Maybe Cell -> Char
drawCell (Just SettledWater) = '~'
drawCell (Just RunningWater) = '|'
drawCell (Just Clay)         = '#'
drawCell Nothing             = ' '

drawWorld :: World -> String
drawWorld things =
  drawAsciiGrid
    (boundingPoint . M.keys $ things)
    (drawCell . (`M.lookup` things))

parseLine :: Parser [AsciiPoint]
parseLine = do
  a <- oneOf "xy"
  symbol "="
  aVal <- integer
  symbol ","
  _ <- oneOf "xy"
  symbol "="
  bMin <- integer
  symbol ".."
  bMax <- integer
  return $
    AsciiPoint .
    (if a == 'x'
       then (aVal, )
       else (, aVal)) <$>
    [bMin .. bMax]

-- flow water down from point, updating world.
down :: AsciiPoint -> WorldStep ()
down p = do
  maxY <- lift ask
  isSet <- gets $ isJust . M.lookup p
  if apY p > maxY || isSet
    then return ()
    else do
      modify' (M.insert p RunningWater)
      down downOne
      downNow <- gets $ M.lookup downOne
      case downNow of
        Just Clay         -> out p
        Just SettledWater -> out p
        _                 -> return ()
  where
    downOne = movePoint South p

data Edge
  = Wall Cell
  | Overspill
  deriving (Eq)

findEdge ::
     (AsciiPoint -> AsciiPoint) -> AsciiPoint -> WorldStep (AsciiPoint, Edge)
findEdge iter p = gets (M.lookup (movePoint South p)) >>= \case
    Nothing -> return (p, Overspill)
    Just RunningWater -> return (p, Wall RunningWater)
    _ -> gets (M.lookup p) >>= \case
        Just Clay -> return (p, Wall Clay)
        _         -> findEdge iter (iter p)

fill :: AsciiPoint -> AsciiPoint -> Cell -> WorldStep ()
fill a b c = modify' $ flip (foldr (`M.insert` c)) $ rectanglePoints a b

-- spread out water horizontally until it finds a wall or spills over.
out :: AsciiPoint -> WorldStep ()
out p = do
  (pl, wl) <- findEdge (movePoint West) p
  (pr, wr) <- findEdge (movePoint East) p
  let fill' = fill (movePoint East pl) (movePoint West pr)
  case (wl, wr) of
    (Wall Clay, Wall Clay) -> fill' SettledWater
    _ -> do
      fill' RunningWater
      when (wl == Overspill) (down pl)
      when (wr == Overspill) (down pr)
