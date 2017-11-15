module Average where

import Data.List (foldl')

import Exchange
import Debug.Trace

-- | n-day Simple Moving Averages
averages :: Fractional a => Int -> [a] -> [a]
averages days = reverse . fst . foldl' stepAvg ([], [])
  where
    stepAvg :: Fractional a => ([a], [a]) -> a -> ([a], [a])
    stepAvg (avgs, previousN) entry =
        let prevLen    = length previousN
            avg        = simpleAverage (prevLen+1) $ entry : previousN
            previousN' = if prevLen < days-1
                then entry : previousN
                else entry : take (prevLen-1) previousN
        in (avg:avgs, previousN')

simpleAverage :: Fractional a => Int -> [a] -> a
simpleAverage _   [] = 0
simpleAverage len xs = foldr (+) 0 xs / realToFrac len
