{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import qualified Data.ByteString.Lazy.Char8 as B8
import Data.Time.Calendar (fromGregorian)
import Data.Aeson (decode, encode)

import Exchange
import Average
import Graph

main :: IO ()
main = do
    entries <- getEntries
    let ints = findAverageIntersections 10 20 entries 
    (flip mapM_) ints $ \intersect -> do
        let entry = entries !! fromIntegral (intersectionIndex intersect)
        putStrLn (show (intersectionOrientation intersect) ++ " " ++ show (entryDate entry))
--print =<< fmap (averages 3 . map entryAverage) getEntries
--getEntries >>= print
--print $ Entry (fromGregorian 2015 12 30) 1 2 3 4 5


findAverageIntersections :: Int -> Int -> [DailySummary] -> [Intersection Bitcoin]
findAverageIntersections daysA daysB entries =
    let dailyAverages = map entryAverage entries
        averagesA     = averages daysA dailyAverages
        averagesB     = averages daysB dailyAverages
    in intersections averagesA averagesB

getEntries :: IO [DailySummary]
getEntries = do
    mentries <- parseQuandlEntries <$> B8.readFile "./data/quandl-example-5000row.json"
    return $ case mentries of
        Just entries -> reverse $ entryData entries
        Nothing      -> []
