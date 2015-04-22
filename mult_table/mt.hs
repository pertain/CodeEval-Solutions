{- mt.hs
 -
 - By William Ersing
 -
 - This is a programming challenge from CodeEval.com.
 - It prints a 12x12 multiplication table with each
 - column aligned. (each number is right aligned
 - and allotted four character spaces).
 -}

import Text.Printf

main :: IO ()
main = do
    printTable $ multTable 12

-- create a nxn multiplication table of
multTable :: Int -> [[Int]]
multTable n = [[x*y | x <- [1..n]] | y <- [1..n]]

-- print the table with correct formatting
printTable :: PrintfArg a => [[a]] -> IO ()
printTable table = mapM_ printRow table
    where
        printRow row = mapM_ (printf "%4d") row >> putStrLn ""
