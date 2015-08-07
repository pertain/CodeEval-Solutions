--{-# LANGUAGE ParallelListComp #-}

--testIterate = [if hi == "L" then x else y | x <- [1,1,1,1] | y <- [4,4,4,4] | hi <- ["H","L","H","H"]]

--getMiddle :: Int -> Int -> String -> Int
--getMiddle roof floor hiLo
--getMiddle hiLo floor roof
    --i| hiLo == "Higher"  = floor + distance + 1
    --i| hiLo == "Lower"   = roof - distance
    --where
        --realFloor   = floor + 1
        --realRoof    = roof - 1
        --distance    = (roof - floor) `div` 2

import Data.List.Split (splitPlaces)

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
{-
testLo :: Int -> Int -> [Int]
testLo floor roof
    | even len = head (last evenSplit) : testLo floor newRoof
    | otherwise = head (oddSplit !! 1) : testLo floor newRoof
    where
        fullRange   = [floor .. roof]
        len         = length fullRange
        half        = len `div` 2
        evenSplit   = splitPlaces [half, half] fullRange
        oddSplit    = splitPlaces [half, 1, half] fullRange
        newRoof     = last $ head evenSplit
-}
