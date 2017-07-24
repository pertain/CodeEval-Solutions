{- ts.hs
 -
 - By William Ersing
 -
 - CodeEval challenge (MODERATE) Trailing String
 - https://www.codeeval.com/open_challenges/32/
 -}

import System.Environment (getArgs)
import Data.List.Split (splitOn)

main :: IO ()
main = do
    inFile <- getArgs
    file <- readFile $ head inFile
    let linesList = lines file
    mapM_ putStrLn $ evalAllLines linesList

parseLine :: String -> (String,String)
parseLine s
    | diffLen <= 0  = (stringA,stringB)
    | otherwise     = (endOfStringA,stringB)
    where
        ss              = splitOn "," s
        stringA         = head ss
        stringB         = last ss
        diffLen         = length stringA - length stringB
        endOfStringA    = drop diffLen stringA

evalLine :: (String,String) -> Int
evalLine t
    | uncurry (==) t    = 1
    | otherwise         = 0

evalAllLines :: [String] -> [String]
evalAllLines = foldr (\x acc ->
                        if null x
                        then acc
                        else show (evalLine (parseLine x)) : acc
                        ) []
