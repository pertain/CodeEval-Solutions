{- si.hs
 -
 - By William Ersing
 -
 - CodeEval challenge (Moderate): Stack Implementation
 - https://www.codeeval.com/open_challenges/9/
 -}

import System.Environment (getArgs)
import Data.List.Split (splitOn)

main :: IO ()
main = do
    inFile <- getArgs
    file <- readFile $ head inFile
    let linesList = pushAll $ lines file
    mapM_ putStrLn $ popAllLines linesList

push :: String -> [String]
push = reverse . splitOn " "

pushAll = map push

pop :: [String] -> String
pop []      = []
pop [x]     = x
pop (x:_)   = x

popEveryOther :: [String] -> [String]
popEveryOther = map pop . takeWhile (not . null) . iterate (drop 2)

popAllLines :: [[String]] -> [String]
popAllLines = map (unwords . popEveryOther)
