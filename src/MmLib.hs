module MmLib where

import Control.Applicative
import qualified Text.Trifecta as T
import Data.List
import System.Random

data Color =
    Red | Blue | Green | Pink | Yellow | Turkis
    deriving (Eq, Show, Enum, Bounded)

instance Random Color where
    randomR (lo, hi) gen = (toEnum val, gen')
        where (val, gen') = randomR (fromEnum lo, fromEnum hi) gen
    random = randomR (minBound, maxBound)

color :: T.Parser Color
color = do
    color' <- T.oneOf "RBGPYT"
    case color' of
        'R' -> return Red
        'B' -> return Blue
        'G' -> return Green
        'P' -> return Pink
        'Y' -> return Yellow
        'T' -> return Turkis
        _ -> empty

type Row = (Color, Color, Color, Color)

randomRow :: IO Row
randomRow = do
    a <- randomIO
    b <- randomIO
    c <- randomIO
    d <- randomIO
    return (a, b, c, d)

row :: T.Parser Row
row = do
    a <- color
    b <- color
    c <- color
    d <- color
    return (a, b, c, d)

parseRow :: String -> T.Result Row
parseRow = T.parseString row mempty

intersectWithoutDuplicates :: Eq a => [a] -> [a] -> [a]
intersectWithoutDuplicates xs ys = xs \\ (xs \\ ys)

data Result = Result {
    numRightPositions :: Int
  , numRightColors :: Int
} deriving (Eq, Show)

check :: Row -> Row -> Result
check (a, b, c, d) (e, f, g, h) =
    let xs = [ a, b, c, d ]
        ys = [ e, f, g, h ]
        zipped = zip xs ys
        rightPositions = filter (\x -> fst x == snd x) $ zipped
        wrongPositions = filter (\x -> fst x /= snd x) $ zipped
        wrongPositionsXs = map fst wrongPositions
        wrongPositionsYs = map snd wrongPositions
        rightColors = intersectWithoutDuplicates wrongPositionsXs wrongPositionsYs
    in Result { numRightPositions = length rightPositions, numRightColors = length rightColors }
