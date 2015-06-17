{- lc.hs
 -
 - By William Ersing
 -
 - CodeEval challenge (EASY) Lowercase
 - https://www.codeeval.com/open_challenges/20/
 -}

import System.Environment (getArgs)
import Data.Char (toLower)

main :: IO ()
main = do
    inFile <- getArgs
    file <- readFile $ head inFile
    let linesList = lines file
    mapM_ putStrLn $ lowerAll linesList

lowerAll :: [String] -> [String]
lowerAll = map (map toLower)
