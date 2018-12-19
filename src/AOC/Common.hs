-- |
-- Module      : AOC.Challenge
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Meant to be a place to include common functionality used across
-- different parts in the challenge.
--
module AOC.Common where

import           Control.Arrow              (first, left, second)
import           Control.Monad              (void)
import qualified Data.Map.Strict            as M
import qualified Data.Set                   as S
import           Data.Time
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Error      (errorBundlePretty)

parseEither :: Parser a -> String -> Either String a
parseEither p i = left errorBundlePretty $ Text.Megaparsec.parse p "Input" i

parseManyEither :: Parser a -> String -> Either String [a]
parseManyEither p = parseEither (Text.Megaparsec.many p)

findFirstDup :: (Ord a) => [a] -> Maybe a
findFirstDup = go S.empty
  where
    go _ [] = Nothing
    go seen (x:xs)
      | x `S.member` seen = Just x
      | otherwise = go (S.insert x seen) xs

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser ()
symbol = void <$> L.symbol sc

integer :: Integral a => Parser a
integer = lexeme (L.signed sc L.decimal)

time :: Parser UTCTime
time = do
  year <- L.decimal
  char '-'
  month <- L.decimal
  char '-'
  day <- L.decimal :: Parser Int
  char ' '
  hour <- L.decimal :: Parser Int
  char ':'
  minute <- L.decimal :: Parser Int
  let date = fromGregorian year month day
  return $ UTCTime date (fromIntegral $ (hour * 60 + minute) * 60)

getOnly :: [a] -> Maybe a
getOnly [x] = Just x
getOnly _   = Nothing

asPairs :: [a] -> [(a, a)]
asPairs []       = []
asPairs [_]      = []
asPairs (x:y:xs) = (x, y) : asPairs xs

breakOn :: (a -> Bool) -> [a] -> [[a]]
breakOn _ [] = []
breakOn p (x:xs)
  | p x =
    let (a, b) = break p xs
     in (x : a) : breakOn p b
  | otherwise = breakOn p xs

newtype AsciiPoint = AsciiPoint
  { unAsciiPoint :: (Int, Int)
  } deriving (Eq)

instance Ord AsciiPoint where
  compare (AsciiPoint (x, y)) (AsciiPoint (x', y')) = compare (y, x) (y', x')

instance Show AsciiPoint where
  show (AsciiPoint (x, y)) = show x <> "," <> show y

data Direction
  = North
  | East
  | South
  | West
  deriving (Show)

movePoint :: Direction -> AsciiPoint -> AsciiPoint
movePoint North = AsciiPoint . second pred . unAsciiPoint
movePoint South = AsciiPoint . second succ . unAsciiPoint
movePoint East  = AsciiPoint . first succ . unAsciiPoint
movePoint West  = AsciiPoint . first pred . unAsciiPoint

turnRight :: Direction -> Direction
turnRight North = East
turnRight East  = South
turnRight South = West
turnRight West  = North

turnLeft :: Direction -> Direction
turnLeft = turnRight . turnRight . turnRight

turnAbout :: Direction -> Direction
turnAbout = turnRight . turnRight

asciiArtGrid :: (Char -> Maybe a) -> String -> M.Map AsciiPoint a
asciiArtGrid f s = M.mapMaybe f cells
  where
    cells =
      M.fromList
        [ (AsciiPoint (x, y), c)
        | (y, line) <- zip [0 ..] $ lines s
        , (x, c) <- zip [0 ..] line
        ]
