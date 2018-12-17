{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day10
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 10.  See "AOC.Solver" for the types used in this module!
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
module AOC.Challenge.Day10
  ( day10a
  , day10b
  ) where

import           AOC.Prelude
import qualified Data.Set    as S

day10a :: _ :~> _
day10a =
  MkSol
    { sParse = parseManyMaybe starParser
    , sShow = id
    , sSolve = Just . (\s -> drawAt (findSmallestIx s) s)
    }

day10b :: _ :~> _
day10b =
  MkSol
    { sParse = parseManyMaybe starParser
    , sShow = show
    , sSolve = Just . findSmallestIx
    }

data Star = Star
  { px0 :: Int
  , py0 :: Int
  , vx  :: Int
  , vy  :: Int
  } deriving (Show)

posAt :: Int -> Star -> (Int, Int)
posAt n (Star x y vx vy) = (x + n * vx, y + n * vy)

findSmallestIx :: [Star] -> Int
findSmallestIx stars = localMinimumIx1 (`areaAt` stars) [0 ..]

localMinimumIx1 :: Ord o => (a -> o) -> [a] -> Int
localMinimumIx1 _ [] = error "empty list for lcoalMinimumIx1"
localMinimumIx1 f (a:as) = go 0 a as
  where
    go ix _ [] = ix
    go ix lst (b:bs)
      | f lst < f b = ix
      | otherwise = go (ix + 1) b bs

sizeAt :: Int -> [Star] -> ((Int, Int), (Int, Int))
sizeAt n stars = ((minX, minY), (maxX, maxY))
  where
    poss = S.fromList $ map (posAt n) stars
    (minX, _) = S.findMin poss
    (maxX, _) = S.findMax poss
    minY = minimum $ S.map snd poss
    maxY = maximum $ S.map snd poss

areaAt :: Int -> [Star] -> Int
areaAt n stars = (maxX - minX) * (maxY - minY)
  where
    ((minX, minY), (maxX, maxY)) = sizeAt n stars

drawAt :: Int -> [Star] -> String
drawAt n stars =
  '\n' :
  unlines
    [ [ if s
      then '#'
      else '.'
    | x <- [minX .. maxX]
    , let s = S.member (x, y) poss
    ]
    | y <- [minY .. maxY]
    ]
  where
    poss = S.fromList $ map (posAt n) stars
    ((minX, minY), (maxX, maxY)) = sizeAt n stars

starParser :: Parser Star
starParser = do
  symbol "position=<"
  x0 <- integer
  symbol ","
  y0 <- integer
  symbol "> velocity=<"
  velX <- integer
  symbol ","
  velY <- integer
  symbol ">"
  return $ Star x0 y0 velX velY
