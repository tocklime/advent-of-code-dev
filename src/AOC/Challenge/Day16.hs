{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day16
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 16.  See "AOC.Solver" for the types used in this module!
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
module AOC.Challenge.Day16
  ( day16a
  , day16b
  ) where

import           AOC.Prelude
import           Control.Monad.ST
import           Data.Bits
import           Data.Bool
import qualified Data.Map.Strict     as M
import qualified Data.Set            as S
import qualified Data.Vector.Unboxed as V
import           Text.Megaparsec

day16a :: [Example] :~> Int
day16a =
  MkSol
    { sParse = fmap stepA <$> parseEither parseInput
    , sShow = show
    , sSolve = Just . length . filter id . map ((>= 3) . length . matches)
    }

day16b :: Prob :~> RegMem
day16b =
  MkSol
    { sParse = parseEither parseInput
    , sShow = show . (V.! 0)
    , sSolve =
        Just .
        (\p -> runProgram (solveOptions . reduceOptions . stepA $ p) (stepB p))
    }

type Ign = Int

type Reg = Int

data Val
  = I Int
  | R Reg

data OpName
  = AddR
  | AddI
  | MulR
  | MulI
  | BanR
  | BanI
  | BorR
  | BorI
  | SetR
  | SetI
  | GtIR
  | GtRI
  | GtRR
  | EqIR
  | EqRI
  | EqRR
  deriving (Ord, Eq, Enum, Bounded, Show)

data OpType
  = Add Reg
        Val
        Reg
  | Mul Reg
        Val
        Reg
  | And Reg
        Val
        Reg
  | Or Reg
       Val
       Reg
  | Set Val
        Ign
        Reg
  | GTo Val
        Val
        Reg
  | EQo Val
        Val
        Reg

runProgram :: M.Map Int OpName -> [OpLine] -> RegMem
runProgram opLookup = foldl' step (V.fromList [0, 0, 0, 0])
  where
    step m (i, a, b, c) = evalOp (opcodes a b c M.! (opLookup M.! i)) m

matches :: Example -> S.Set OpName
matches Eg {..} =
  S.fromList [n | (n, o) <- M.toList (opcodes a b c), evalOp o before == after]
  where
    (_, a, b, c) = op

type MapSet k a = M.Map k (S.Set a)

reduceOptions :: [Example] -> MapSet Int OpName
reduceOptions = foldr step initial
  where
    initial :: M.Map Int (S.Set OpName)
    initial = M.fromList . map (, S.fromList [minBound ..]) $ [0 .. 16]
    step :: Example -> M.Map Int (S.Set OpName) -> M.Map Int (S.Set OpName)
    step eg = M.adjust (S.intersection ms) i
      where
        ms = matches eg
        (i, _, _, _) = op eg

solveOptions :: MapSet Int OpName -> M.Map Int OpName
solveOptions = M.map S.findMin . reduce
  where
    reduce :: MapSet Int OpName -> MapSet Int OpName
    reduce m
      | length allSingles == 16 = m
      | otherwise = reduce m'
      where
        allSingles =
          S.fromList . M.elems . M.map S.findMin . M.filter ((== 1) . S.size) $
          m
        m' = M.map removeSingles m
        removeSingles s
          | S.size s == 1 = s
          | otherwise = S.difference s allSingles

opcodes :: Int -> Int -> Int -> M.Map OpName OpType
opcodes a b c =
  M.fromList
    [ (AddR, Add a (R b) c)
    , (AddI, Add a (I b) c)
    , (MulR, Mul a (R b) c)
    , (MulI, Mul a (I b) c)
    , (BanR, And a (R b) c)
    , (BanI, And a (I b) c)
    , (BorR, Or a (R b) c)
    , (BorI, Or a (I b) c)
    , (SetR, Set (R a) b c)
    , (SetI, Set (I a) b c)
    , (GtIR, GTo (I a) (R b) c)
    , (GtRI, GTo (R a) (I b) c)
    , (GtRR, GTo (R a) (R b) c)
    , (EqIR, EQo (I a) (R b) c)
    , (EqRI, EQo (R a) (I b) c)
    , (EqRR, EQo (R a) (R b) c)
    ]

type RegMem = V.Vector Int

readVal :: Val -> RegMem -> Int
readVal (I x) _ = x
readVal (R x) m = m V.! x

writeVal :: Reg -> Int -> RegMem -> RegMem
writeVal r i m = m V.// [(r, i)]

calc :: Val -> Val -> Reg -> (Int -> Int -> Int) -> RegMem -> RegMem
calc a b c op m = writeVal c (op (readVal a m) (readVal b m)) m

toInt :: Bool -> Int
toInt = bool 0 1

evalOp :: OpType -> RegMem -> RegMem
evalOp (Add a b c) = calc (R a) b c (+)
evalOp (Mul a b c) = calc (R a) b c (*)
evalOp (And a b c) = calc (R a) b c (.&.)
evalOp (Or a b c)  = calc (R a) b c (.|.)
evalOp (Set a b c) = calc a (I b) c const
evalOp (GTo a b c) = calc a b c ((toInt .) . (>))
evalOp (EQo a b c) = calc a b c ((toInt .) . (==))

-- evalOp :: OpType -> Int -> Int -> Int ->
type OpLine = (Int, Int, Int, Int)

data Prob = Prob
  { stepA :: [Example]
  , stepB :: [OpLine]
  }

data Example = Eg
  { before :: RegMem
  , op     :: OpLine
  , after  :: RegMem
  } deriving (Show)

parseInput :: Parser Prob
parseInput = do
  egs <- Text.Megaparsec.many parseStepA
  stepB <- Text.Megaparsec.many parseOpLine
  return $ Prob egs stepB

parseOpLine :: Parser OpLine
parseOpLine = (,,,) <$> integer <*> integer <*> integer <*> integer

parseStepA :: Parser Example
parseStepA = do
  symbol "Before: ["
  bef <- V.fromList <$> integer `sepBy` symbol ","
  symbol "]"
  opLine <- parseOpLine
  symbol "After:  ["
  aft <- V.fromList <$> integer `sepBy` symbol ","
  symbol "]"
  return $ Eg bef opLine aft
