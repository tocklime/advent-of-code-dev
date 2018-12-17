module AOC.Challenge.Day03
  ( day03a
  , day03b
  ) where

import           AOC.Common
import           AOC.Solver         ((:~>) (..))
import           Control.Monad      (forM, forM_, when)
import           Control.Monad.ST   (ST, runST)
import           Data.Array.ST      (STArray, STUArray, newArray, readArray,
                                     runSTUArray, writeArray)
import qualified Data.Array.Unboxed as A
import qualified Data.Set           as S
import           Data.STRef         (modifySTRef', newSTRef, readSTRef)
import           Text.Megaparsec    (many, parseMaybe)

data Claim = Claim
  { claimId :: Int
  , left    :: Int
  , top     :: Int
  , width   :: Int
  , height  :: Int
  } deriving (Show, Eq, Ord)

parseProb :: Parser [Claim]
parseProb = Text.Megaparsec.many claim
  where
    symInt x = symbol x *> (fromIntegral <$> integer)
    claim =
      Claim <$> symInt "#" <*> symInt "@" <*> symInt "," <*> symInt ":" <*>
      symInt "x"

covers :: Claim -> [(Int, Int)]
covers (Claim _ l t w h) =
  [(x, y) | x <- [l .. l + w - 1], y <- [t .. t + h - 1]]

toSheet :: [Claim] -> A.UArray (Int, Int) Int
toSheet cs =
  runSTUArray $ do
    sheet <-
      newArray ((0, 0), (1000, 1000)) (0 :: Int) :: ST s (STUArray s (Int, Int) Int)
    forM_ cs $ \c ->
      forM_ (covers c) $ \p -> readArray sheet p >>= writeArray sheet p . succ
    return sheet

countOverused :: A.UArray (Int, Int) Int -> Int
countOverused = length . filter (> 1) . A.elems

findDisconnected :: [Claim] -> [Claim]
findDisconnected cs =
  runST $ do
    sheet <-
      newArray ((0, 0), (1000, 1000)) [] :: ST s (STArray s (Int, Int) [Claim])
    alones <- newSTRef S.empty
    forM_ cs $ \c -> do
      isNew <-
        forM (covers c) $ \p -> do
          claims <- readArray sheet p
          forM_ claims $ \otherclaim ->
            modifySTRef' alones $ S.delete otherclaim
          writeArray sheet p (c : claims)
          return (null claims)
      when (and isNew) . modifySTRef' alones $ S.insert c
    finalAlones <- readSTRef alones
    return (S.toList finalAlones)

day03a :: [Claim] :~> Int
day03a =
  MkSol
    { sParse = parseMaybe parseProb
    , sShow = show
    , sSolve = Just . countOverused . toSheet
    }

day03b :: [Claim] :~> Int
day03b =
  MkSol
    { sParse = parseMaybe parseProb
    , sShow = show
    , sSolve = getOnly . map claimId . findDisconnected
    }
