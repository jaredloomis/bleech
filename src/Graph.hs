module Graph where

import Data.List (foldl')

data Intersection a = Intersection {
    -- | True if first line beat second, False if second beat first
    intersectionOrientation :: !Bool,
    -- | Index in the data where this occurred
    intersectionIndex       :: !Integer,
    -- | Value at the index where intersection occurred
    intersectionValue       :: !a
} deriving (Show, Eq)

intersections :: Ord a => [a] -> [a] -> [Intersection a]
intersections as bs = fstTrip $ foldl' stepInt ([], initialOrientation, 0) pairs
  where
    stepInt :: Ord a => ([Intersection a], Bool, Integer) -> (a, a) -> ([Intersection a], Bool, Integer)
    stepInt (acc, orientation, index) (a, b) =
        let orientation' = a > b
        in if orientation /= orientation'
            then (Intersection orientation' index a : acc, orientation', index+1)
            else (acc, orientation, index+1)

    initialOrientation :: Bool
    initialOrientation
      | length pairs == 0 = False
      | otherwise         =
        let (a1, b1) = head pairs
        in  a1 > b1

    pairs = zip as bs
    fstTrip (x, _, _) = x
