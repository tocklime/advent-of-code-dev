module AOC.Challenge.Day13
  ( day13a
  , day13b
  ) where

import           AOC.MinimalPrelude
import           Control.Arrow             (right)
import qualified Control.Monad.Writer.Lazy as W
import qualified Data.Map.Strict           as M
import           Data.Maybe                (mapMaybe)
import           Safe                      (headMay)

day13a :: Network :~> AsciiPoint
day13a =
  MkSol {sParse = return . readGrid, sShow = show, sSolve = firstCrash . run}

day13b :: Network :~> AsciiPoint
day13b =
  MkSol {sParse = return . readGrid, sShow = show, sSolve = lastCart . run}

data Track
  = Inter
  | CornerSlash
  | CornerBackslash
  deriving (Show)

data IntersectionChoice
  = L
  | Straight
  | R
  deriving (Show)

data Cart = Cart
  { cartDirection        :: Direction
  , cartNextIntersection :: IntersectionChoice
  } deriving (Show)

type CartPos = (AsciiPoint, Cart)

data Network = Net
  { netGrid  :: M.Map AsciiPoint Track
  , netCarts :: M.Map AsciiPoint Cart
  } deriving (Show)

cartMove :: Network -> CartPos -> (Network, Maybe AsciiPoint)
cartMove (Net grid carts) (pos, Cart dir ni)
  | M.notMember pos carts = (Net grid carts, Nothing) -- cart already gone. Ignore.
  | M.member nextPos carts --crash!
   = (Net grid (M.delete nextPos . M.delete pos $ carts), Just nextPos)
  | otherwise =
    (Net grid (M.insert nextPos newCart . M.delete pos $ carts), Nothing)
  where
    nextPos = movePoint dir pos
    nextTrack = M.lookup nextPos grid
    newCart =
      case nextTrack of
        Just x  -> runCorner x (Cart dir ni)
        Nothing -> Cart dir ni

runCorner :: Track -> Cart -> Cart
runCorner Inter (Cart d ni) =
  case ni of
    L        -> Cart (turnLeft d) Straight
    Straight -> Cart d R
    R        -> Cart (turnRight d) L
runCorner CornerSlash (Cart d ni) = Cart d' ni
  where
    d' =
      case d of
        North -> East
        East  -> North
        West  -> South
        South -> West
runCorner CornerBackslash c = Cart (turnAbout d') ni
  where
    (Cart d' ni) = runCorner CornerSlash c

readCell :: Char -> Maybe (Either Track (IntersectionChoice -> Cart))
readCell '/'  = Just . Left $ CornerSlash
readCell '\\' = Just . Left $ CornerBackslash
readCell '+'  = Just . Left $ Inter
readCell '>'  = Just . Right $ Cart East
readCell '<'  = Just . Right $ Cart West
readCell '^'  = Just . Right $ Cart North
readCell 'v'  = Just . Right $ Cart South
readCell _    = Nothing

readGrid :: String -> Network
readGrid t = Net as bs
  where
    (as, bs) = M.mapEither (right ($ L)) $ asciiArtGrid readCell t

firstCrash :: [Event] -> Maybe AsciiPoint
firstCrash = headMay . mapMaybe cp
  where
    cp (Crash x) = Just x
    cp _         = Nothing

lastCart :: [Event] -> Maybe AsciiPoint
lastCart = headMay . mapMaybe lc
  where
    lc (LastCart x) = Just x
    lc _            = Nothing

data Event
  = Crash AsciiPoint
  | LastCart AsciiPoint

tickNetwork :: Network -> W.Writer [Event] Network
tickNetwork n = W.foldM f n (M.toList $ netCarts n)
  where
    f net (pos, c) =
      case cartMove net (pos, c) of
        (net', Just pos') -> do
          W.tell [Crash pos']
          return net'
        (net', Nothing) -> return net'

run :: Network -> [Event]
run = W.execWriter . go
  where
    go :: Network -> W.Writer [Event] Network
    go n = do
      n' <- tickNetwork n
      case M.keys $ netCarts n' of
        [x] -> W.tell [LastCart x] >> return n'
        _   -> go n'
