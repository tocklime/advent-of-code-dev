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

import           Control.Monad              (void)
import qualified Data.Set                   as S
import           Data.Time
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

parseMaybe :: Parser a -> String -> Maybe a
parseMaybe = Text.Megaparsec.parseMaybe

parseManyMaybe :: Parser a -> String -> Maybe [a]
parseManyMaybe = Text.Megaparsec.parseMaybe . Text.Megaparsec.many

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
