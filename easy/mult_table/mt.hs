{- mt.hs
 -
 - By William Ersing
 -
 - This is a programming challenge from CodeEval.com.
 -
 - (EASY) Multiplication Table
 - https://www.codeeval.com/open_challenges/23/
 -
 - =================================================
 -
 - It prints a 12x12 multiplication table with each
 - column aligned. (each number is right aligned
 - and allotted four character spaces).
 -
 - =================================================
 -}

import Text.Printf

main :: IO ()
main = printTable $ multTable 12

-- create a nxn multiplication table of
multTable :: Int -> [[Int]]
multTable n = [[x*y | x <- [1..n]] | y <- [1..n]]

-- print the table with correct formatting
printTable :: PrintfArg a => [[a]] -> IO ()
printTable = mapM_ printRow
    where
        printRow row = mapM_ (printf "%4d") row >> putStrLn ""
