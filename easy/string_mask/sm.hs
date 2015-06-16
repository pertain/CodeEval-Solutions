{- sm.hs
 -
 - By William Ersing
 -
 - CodeEval challenge (EASY) String Mask
 - https://www.codeeval.com/open_challenges/199/
 -}

import System.Environment (getArgs)
import Data.Char (toUpper)

main :: IO ()
main = do
    inFile <- getArgs
    file <- readFile $ head inFile
    let linesList = lines file
    mapM_ putStrLn $ maskAllLines linesList

prepareTuples :: String -> [(Char,Char)]
prepareTuples s = zip (head x) (last x)
    where
        x = words s

maskString :: [(Char,Char)] -> String
maskString ct = [ if snd x == '1' then toUpper (fst x) else fst x | x <- ct ]

maskAllLines :: [String] -> [String]
maskAllLines = map (maskString . prepareTuples)
