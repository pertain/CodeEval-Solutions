{- saa.hs
 -
 - By William Ersing
 -
 - CodeEval challenge (EASY) Strings and Arrows
 - https://www.codeeval.com/open_challenges/203/
 -}

import System.Environment (getArgs)

main :: IO ()
main = do
    inFile <- getArgs
    file <- readFile $ head inFile
    let linesList = lines file
    mapM_ putStrLn $ allArrows linesList

allSubsOfN :: Int -> String -> [String]
allSubsOfN n a@(x:xs)
    | length a < n  = []
    | otherwise     = take n a : allSubsOfN n xs

countArrows :: [String] -> Int
countArrows = foldr (\x acc -> if x == "<--<<" || x == ">>-->" then acc + 1 else acc) 0

allArrows :: [String] -> [String]
allArrows = map (show . countArrows . allSubsOfN 5)
