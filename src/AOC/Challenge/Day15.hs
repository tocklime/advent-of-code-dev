-- |
-- Module      : AOC.Challenge.Day15
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 15.  See "AOC.Solver" for the types used in this module!
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
module AOC.Challenge.Day15
  ( day15a
  , day15b
  , drawWorld15
  ) where

import           AOC.MinimalPrelude
import           Control.Monad.Identity
import           Control.Monad.State
import           Control.Monad.Trans.Except
import           Data.Graph.AStar           (aStar)
import qualified Data.HashSet               as HS
import qualified Data.Map.Strict            as M
import           Data.Maybe                 (mapMaybe)
import           Data.Ord                   (comparing)
import qualified Data.Set                   as S
import           Debug.Trace
import           Safe

day15a :: World :~> Result
day15a =
  MkSol
    { sParse = return . readGrid
    , sShow = \Result {..} -> show $ turnCount * remainingHealth
    , sSolve = Just . runWorld
    }

day15b :: World :~> Result
day15b =
  MkSol
    { sParse = return . readGrid
    , sShow = \Result {..} -> show $ turnCount * remainingHealth
    , sSolve = Just . snd . minimumTechUpgrade
    }

data Faction
  = Goblin
  | Elf
  deriving (Eq, Show)

data Thing
  = Fighter { fighterFaction :: Faction
            , fighterHealth  :: Int }
  | Wall
  deriving (Show)

readCell :: Char -> Maybe Thing
readCell '#' = Just Wall
readCell 'E' = Just $ Fighter Elf 200
readCell 'G' = Just $ Fighter Goblin 200
readCell _   = Nothing

data World = World
  { things         :: M.Map AsciiPoint Thing
  , age            :: Int
  , stopOnElfDeath :: Bool
  , elfPower       :: Int
  }

thingStats :: (AsciiPoint, Thing) -> Maybe String
thingStats (p, Fighter Goblin x) = Just $ "G" <> show x <> "@" <> show p
thingStats (p, Fighter Elf x)    = Just $ "E" <> show x <> "@" <> show p
thingStats _                     = Nothing

drawWorld15 :: World -> String
drawWorld15 World {..} =
  unlines $
  ("The world at age " <> show age <> " stop on elf death: " <>
   show stopOnElfDeath <>
   " elf Power: " <>
   show elfPower) :
  [ [ case M.lookup p things of
    Just Wall               -> '#'
    Just (Fighter Goblin _) -> 'G'
    Just (Fighter Elf _)    -> 'E'
    Nothing                 -> '.'
  | x <- [0 .. maxX]
  , let p = AsciiPoint (x, y)
  ]
  | y <- [0 .. maxY]
  ] ++
  ["Actors: " <> unwords (mapMaybe thingStats $ M.toList things)]
  where
    maxX = maximum . map (fst . unAsciiPoint) . M.keys $ things
    maxY = maximum . map (snd . unAsciiPoint) . M.keys $ things

readGrid :: String -> World
readGrid = (\x -> World x 0 False 3) . asciiArtGrid readCell

data UnitAction
  = EndCombat
  | NoMove
  | Move AsciiPoint
  deriving (Show)

isFighter :: Thing -> Bool
isFighter Fighter {} = True
isFighter _          = False

isGoblin :: Thing -> Bool
isGoblin (Fighter Goblin _) = True
isGoblin _                  = False

data Result = Result
  { winner          :: Faction
  , remainingHealth :: Int
  , turnCount       :: Int
  } deriving (Show)

minimumTechUpgrade :: World -> (Int, Result)
minimumTechUpgrade w = go 4
  where
    go n =
      let r = test n
       in if elfWin r
            then (n, r)
            else go (n + 1)
    test = log2 . runWorld . mkWorld . log1
    log1 n = trace ("Trying elf power " <> show n) n
    log2 r = trace ("Result: " <> show r) r
    mkWorld x = w {stopOnElfDeath = True, elfPower = x}
    elfWin (Result Elf _ _) = True
    elfWin _                = False

--  binarySearchUnbounded 3 test elfWin
runWorld :: World -> Result
runWorld w = (\(Right i, _) -> i) $ runWorldAction w go
  where
    go = do
      done <- stepWorld
      if done
        then gets scoreWorld
        else go

type WorldAction a = ExceptT () (StateT World Identity) a

runWorldAction :: World -> WorldAction a -> (Either () a, World)
runWorldAction w = runIdentity . flip runStateT w . runExceptT

scoreWorld :: World -> Result
scoreWorld World {..} = Result w (sum . map health . M.elems $ things) (age - 1)
  where
    w =
      if M.null . M.filter isGoblin $ things
        then Elf
        else Goblin

stepWorld :: WorldAction Bool
stepWorld = do
  actors <- M.keys . M.filter isFighter <$> gets things
  (_, done) <- foldM unitTurn (S.empty, False) actors
  modify' (\w' -> w' {age = age w' + 1})
  return done
  where
    unitTurn (alreadyMoved, done) a =
      if S.member a alreadyMoved -- corner case where G1 kills E, G2 moves right to where E was, then G2 moves again instead of E.
        then return (alreadyMoved, done)
        else do
          (newPos, noTargets) <- unitMoveTurn a
          deadUnit <- unitAttackTurn newPos
          deadElf <-
            case deadUnit of
              Just Elf -> gets stopOnElfDeath
              _        -> return False
          return (S.insert newPos alreadyMoved, deadElf || done || noTargets)

health :: Thing -> Int
health (Fighter _ x) = x
health _             = 0

hit :: Int -> Thing -> Maybe Thing
hit x (Fighter f h)
  | h' > 0 = Just $ Fighter f h'
  | otherwise = Nothing
  where
    h' = h - x
hit _ x = Just x

unitAttackTurn :: AsciiPoint -> WorldAction (Maybe Faction)
unitAttackTurn ascp = do
  mme <- gets (M.lookup ascp . things)
  case mme of
    Just (Fighter myF _) -> do
      let nearMe = neighbourhood ascp
      w <- gets things
      let nearTargets =
            mapMaybe
              (\p ->
                 case M.lookup p w of
                   Just t@(Fighter theirF _) ->
                     if theirF /= myF
                       then Just (p, t)
                       else Nothing
                   _ -> Nothing)
              nearMe
      case minimumByMay (comparing (health . snd)) nearTargets of
        Just (p, fi) -> do
          power <-
            if myF == Elf
              then gets elfPower
              else return 3
          let fi' = hit power fi
          modify' (\w' -> w' {things = M.update (const fi') p (things w')})
          return $
            if health fi <= power
              then Just $ fighterFaction fi
              else Nothing
        _ -> return Nothing
    _ -> return Nothing

unitMoveTurn :: AsciiPoint -> WorldAction (AsciiPoint, Bool)
unitMoveTurn startPos = do
  w <- gets things
  case M.lookup startPos w of
    Just me@(Fighter myFaction _) -> do
      targets <-
        gets
          (M.keys .
           M.filter ((/= myFaction) . fighterFaction) .
           M.filter isFighter . things)
      if null targets
        then return (startPos, True)
        else do
          let allPossibleAttackPos =
                S.fromList (concatMap neighbourhood targets)
          if startPos `S.member` allPossibleAttackPos
            then return (startPos, False)
            else do
              let clearAttackPositions =
                    S.filter (`M.notMember` w) allPossibleAttackPos
              if null clearAttackPositions
                then return (startPos, False)
                else do
                  let path =
                        minimumByMay (comparing length) .
                        mapMaybe (shortestPath (M.keysSet w) startPos) .
                        S.toList $
                        clearAttackPositions
                  case path of
                    (Just (x:_)) -> do
                      modify'
                        (\w' ->
                           w'
                             { things =
                                 M.insert x me . M.delete startPos . things $ w'
                             })
                      return (x, False)
                    _ -> return (startPos, False)
    _ -> return (startPos, False)

data MyCost =
  Cost Int
       Int
  deriving (Eq, Ord)

instance Num MyCost where
  (Cost a b) + (Cost c d) = Cost (a + b) (c + d)
  (Cost a b) - (Cost c d) = Cost (a - b) (c - d)
  (Cost a b) * (Cost c d) = Cost (a * b) (c * d)
  abs (Cost a b) = Cost (abs a) (abs b)
  signum (Cost a _) = Cost (signum a) 0
  fromInteger a = Cost (fromIntegral a) 0

shortestPath ::
     S.Set AsciiPoint -> AsciiPoint -> AsciiPoint -> Maybe [AsciiPoint]
shortestPath obstacles start goal =
  aStar neighbours costPair costToGoal (== goal) start
    --always prefer going up, then left, then right, then down.
  where
    costPair a@(AsciiPoint (x, y)) b@(AsciiPoint (x', y')) =
      Cost (manhattanDistance a b) fiddle
      where
        fiddle
          | y' < y = 0
          | y' > y = 3
          | x' < x = 1
          | x' > x = 2
          | otherwise = 4
    costToGoal a = costPair a goal
    neighbours = HS.fromList . filter (`S.notMember` obstacles) . neighbourhood
