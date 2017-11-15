module Slice where

import Exchange

data Slice = Slice {
    sliceItem :: Entry,
    averages  :: Map Int Bitcoin
}

createSlices :: [Entry] -> [Slice]
createSlices = undefined
