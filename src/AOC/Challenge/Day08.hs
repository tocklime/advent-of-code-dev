{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day08
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 8.  See "AOC.Solver" for the types used in this module!
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
module AOC.Challenge.Day08
  ( day08a
  , day08b
  ) where

import           AOC.MinimalPrelude
import           Control.Monad      (replicateM)

day08a :: _ :~> _
day08a =
  MkSol
    {sParse = parseEither nodeParser, sShow = show, sSolve = Just . sumMetadata}

day08b :: _ :~> _
day08b =
  MkSol {sParse = parseEither nodeParser, sShow = show, sSolve = Just . value}

data Node =
  Node [Node]
       [Int]

sumMetadata :: Node -> Int
sumMetadata (Node c m) = sum (map sumMetadata c ++ m)

value :: Node -> Int
value (Node [] ms) = sum ms
value (Node cs ms) = sum $ map value goodReferences
  where
    cLen = length cs
    goodReferences =
      map (\x -> cs !! (x - 1)) . filter (\x -> x > 0 && x <= cLen) $ ms

nodeParser :: Parser Node
nodeParser = do
  cCount <- integer
  mCount <- integer
  cs <- replicateM cCount nodeParser
  ms <- replicateM mCount integer
  return $ Node cs ms
