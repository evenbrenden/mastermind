module MmLib where

import Data.List
import System.Random

data Color =
    Red | Blue | Green | Pink | Yellow | Turkis
    deriving (Eq, Show, Enum, Bounded)

instance Random Color where
    randomR (lo, hi) gen = (toEnum val, gen')
        where (val, gen') = randomR (fromEnum lo, fromEnum hi) gen
    random = randomR (minBound, maxBound)

type Row = (Color, Color, Color, Color)

randomRow :: IO Row
randomRow = do
    a <- randomIO
    b <- randomIO
    c <- randomIO
    d <- randomIO
    return (a, b, c, d)

intersectWithoutDuplicates :: Eq a => [a] -> [a] -> [a]
intersectWithoutDuplicates xs ys = xs \\ (xs \\ ys)

check :: Row -> Row -> (Int, Int)
check (a, b, c, d) (e, f, g, h) =
    let xs = [ a, b, c, d ]
        ys = [ e, f, g, h ]
        zipped = zip xs ys
        rightPositions = filter (\x -> fst x == snd x) $ zipped
        numRightPositions = length rightPositions
        wrongPositions = filter (\x -> fst x /= snd x) $ zipped
        wrongPositionsXs = map fst wrongPositions
        wrongPositionsYs = map snd wrongPositions
        rightColors = intersectWithoutDuplicates wrongPositionsXs wrongPositionsYs
        numRightColors = length rightColors
    in (numRightPositions, numRightColors)
