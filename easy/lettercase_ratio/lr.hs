{- lr.hs
 -
 - By William Ersing
 -
 - CodeEval challenge (EASY) Lettercase Percentage Ratio
 - https://www.codeeval.com/open_challenges/147/
 -}

import System.Environment (getArgs)
import Data.List (sortBy, groupBy)
import Data.Char (isUpper, isLower)
import Data.Function (on)
import Text.Printf

main :: IO ()
main = do
    inFile <- getArgs
    file <- readFile $ head inFile
    let linesList = lines file
    mapM_ putStrLn $ caseRatioFile linesList

groupByCase :: String -> [String]
groupByCase = groupBy ((==) `on` isLower) . sortBy (compare `on` isLower)

printFmtLn :: Float -> Float -> String
printFmtLn n = printf "%s %2.2f %s %2.2f" "lowercase:" n "uppercase:"

caseRatioLine :: String -> String
caseRatioLine s
    | all isUpper flatGrp   = printFmtLn 0.0 100.0
    | all isLower flatGrp   = printFmtLn 100.0 0.0
    | otherwise             = printFmtLn ucr lcr
    where
        grps    = groupByCase s
        flatGrp = concat grps
        totL    = length $ head grps
        totU    = length $ last grps
        total   = totL + totU
        lcr     = 100 * fromIntegral totL / fromIntegral total
        ucr     = 100 * fromIntegral totU / fromIntegral total

caseRatioFile :: [String] -> [String]
caseRatioFile = map caseRatioLine
