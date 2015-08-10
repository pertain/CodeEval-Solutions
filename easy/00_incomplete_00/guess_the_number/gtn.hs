import Data.List.Split (splitPlaces)

{-
getHigherMiddle :: Int -> Int -> Int
getHigherMiddle floor roof = floor + distance
    where
        distance    = round (fromIntegral (roof - floor) / 2)

getLowerMiddle :: Int -> Int -> Int
getLowerMiddle floor roof = roof - distance
    where
        distance    = round (fromIntegral (roof - floor) / 2)


getHiMid :: Int -> Int -> [Int]
getHiMid floor roof = floor + distance : getHiMid newFloor roof
    where
        distance    = round (fromIntegral (roof - floor) / 2)
        newFloor    = floor + distance + 1

getLoMid :: Int -> Int -> [Int]
getLoMid floor roof = roof - distance : getLoMid floor newRoof
    where
        distance    = round (fromIntegral (roof - floor) / 2)
        newRoof     = roof - distance


-- this is the most recent portion -- continue testing here
robusto :: String -> Int -> Int -> [Int]
robusto hiLo floor roof
    | hiLo == "Higher" && not (null $ head evenSplit) =
        if even len
        then head (last evenSplit) : robusto "Higher" newBound roof
        else head (oddSplit !! 1) : robusto "Higher" newBound roof
    | hiLo == "Lower" && not (null $ head evenSplit) =
        if even len
        then head (last evenSplit) : robusto "Lower" floor newBound
        else head (oddSplit !! 1) : robusto "Lower" floor newBound
    | otherwise = []
    where
        fullRange   = [floor .. roof]
        len         = length fullRange
        half        = len `div` 2
        evenSplit   = splitPlaces [half, half] fullRange
        oddSplit    = splitPlaces [half, 1, half] fullRange
        newBound    = last $ head evenSplit
-}

-- UNDER CONSTRUCTION -- NOT QUITE THERE YET
lowerMid :: Int -> Int -> Int
lowerMid lo hi
    | evenLen   = evenDist
    | otherwise = oddDist
    where
        len         = length [lo .. hi]
        evenLen     = even len
        distance    = (hi - lo) `div` 2
        evenDist    = roof - distance + 1
        oddDist     = 

-- UNDER CONSTRUCTION -- NOT QUITE THERE YET
higherMid :: Int -> Int -> Int
higherMid lo hi
    | evenLen   = evenDist
    | otherwise = oddDist
    where
        len         = length [lo .. hi]
        evenLen     = even len
        distance    = (hi - lo) `div` 2
        evenDist    = lo + distance + 1
        oddDist     = lo + distance
