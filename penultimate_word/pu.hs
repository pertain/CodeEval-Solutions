{- pu.hs
 -
 - By William Ersing
 -
 - This is a programming challenge from CodeEval.com.
 - It reads in a file with each line containing a
 - string of words. The goal is to locate the
 - penultimate word (second from last) in each string
 - and print it to stdout.
 -
 - Example:
 -
 -  "I loaf and lean at my ease"
 -
 -      The penultimate word is: "my"
 -}

import System.Environment (getArgs)

main :: IO ()
main = do
    inFile <- getArgs
    file <- readFile $ head inFile
    let linesList = lines file
    mapM_ putStrLn $ penultimateWordsList linesList

-- yields the penultimate word in a string of words
penultimateWord :: [String] -> String
penultimateWord s
	| null s		= []
	| length s <= 2 = head s
	| otherwise     = last $ init s

-- yields list of penultimate words from list of strings
penultimateWordsList :: [String] -> [String]
penultimateWordsList = map (penultimateWord . words)
