module MmLib where

import Data.List
import System.Random

data Color =
    Red | Blue | Green | Pink | Yellow | Turkis
    deriving (Eq, Ord, Show, Enum, Bounded)

instance Random Color where
    randomR (lo, hi) gen = (toEnum val, gen')
        where (val, gen') = randomR (fromEnum lo, fromEnum hi) gen
    random = randomR (minBound, maxBound)

data Row =
    Row (Color, Color, Color, Color)
    deriving (Eq, Ord, Show)

randomRow :: IO Row
randomRow = do
    a <- randomIO
    b <- randomIO
    c <- randomIO
    d <- randomIO
    return $ Row (a, b, c, d)

intersectWithoutDuplicates :: Eq a => [a] -> [a] -> [a]
intersectWithoutDuplicates xs ys = xs \\ (xs \\ ys)

data Result = Result {
    numRightPositions :: Int
  , numRightColors :: Int
} deriving (Eq, Ord, Show)

check :: Row -> Row -> Result
check (Row (a, b, c, d)) (Row (e, f, g, h)) =
    let xs = [ a, b, c, d ]
        ys = [ e, f, g, h ]
        zipped = zip xs ys
        rightPositions = filter (\x -> fst x == snd x) $ zipped
        wrongPositions = filter (\x -> fst x /= snd x) $ zipped
        wrongPositionsXs = map fst wrongPositions
        wrongPositionsYs = map snd wrongPositions
        rightColors = intersectWithoutDuplicates wrongPositionsXs wrongPositionsYs
    in Result { numRightPositions = length rightPositions, numRightColors = length rightColors }
