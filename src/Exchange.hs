{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Exchange where

import Control.Applicative (liftA3)

import qualified Data.Vector as V ((!))

import qualified Data.Text      as T
import qualified Data.Text.Read as TR

import qualified Data.ByteString.Lazy       as B

import Data.Time.LocalTime (LocalTime)
import Data.Time.Calendar (Day, fromGregorian)
import Data.Time.Format (parseTimeM, defaultTimeLocale)

import Data.Aeson (FromJSON(..), (.:), withArray, decode, Object, Value(..))
import Data.Aeson.Types (Result(..), Parser, parse)
import Data.Aeson.TH (deriveJSON, defaultOptions)

type Bitcoin = Double

data Entry = Entry {
    entryTime  :: !LocalTime,
    entryPrice :: !Bitcoin
}

--    SimpleDailySummary     ::
data DailySummary =
    DailySummary {
        entryDate   :: !Day,
        entryOpen   :: !Bitcoin,
        entryHigh   :: !Bitcoin,
        entryLow    :: !Bitcoin,
        entryClose  :: !Bitcoin,
        entryVolume :: !Bitcoin
    } deriving (Show, Eq)

parseQuandl :: Value -> Parser DailySummary
parseQuandl = withArray "DailySummary" $ \v ->
        let mentry = DailySummary
                <$> processDate   (v V.! 0)
                <*> processDouble (v V.! 1)
                <*> processDouble (v V.! 2)
                <*> processDouble (v V.! 3)
                <*> processDouble (v V.! 4)
                <*> processDouble (v V.! 5)
        in case mentry of
            Just entry -> return entry :: Parser DailySummary
            Nothing    -> mempty
      where
        processDate :: Value -> Maybe Day
        processDate (String val) =
            let components  = T.split (== '-') val
                decimal     = fmap fst . TR.decimal
                myear       = fmap fromIntegral . decimal $ components !! 0
                mmonth      =                     decimal $ components !! 1
                mday        =                     decimal $ components !! 2
            in case liftA3 fromGregorian myear mmonth mday of
                Right day -> Just day
                Left  _   -> Nothing
        processDate _            = Nothing

        processDouble :: Value -> Maybe Double
        processDouble (Number number) = Just . realToFrac $ number
        processDouble _               = Nothing

-- TODO for real
summaryAverage :: DailySummary -> Bitcoin
summaryAverage = entryHigh

data RawSummaries = RawSummaries {
    entryData :: [DailySummary]
} deriving (Show, Eq)

instance FromJSON RawSummaries where
    parseJSON (Object v) =
        RawSummaries <$> v .: "data"

parseQuandlEntries :: B.ByteString -> Maybe [DailySummary]
parseQuandlEntries str =
    let mjson = decode str :: Maybe Object
    in case mjson of
        Just json -> case parse (const $ (json .: "data") >>= parseQuandl) () of
            Success summaries -> Just summaries
            Error   _         -> Nothing
        Nothing   -> Nothing
            
--(parseJSON . (.: "data")) =<< mjson
--parseJSON --decode
