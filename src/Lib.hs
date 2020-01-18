module Lib where

import Data.List

data Color =
    Red | Blue | Green | Pink | Yellow | Turkis
    deriving (Eq, Show)

type Row = (Color, Color, Color, Color)

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
