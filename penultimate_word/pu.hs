module Main where
import System.Environment (getArgs)
import Data.Char

main = do
	[inFile] <- getArgs
	file <- readFile inFile
	let linesList = lines file
	mapM_ putStrLn $ penultimateWordsList linesList

-- yields the penultimate word in a string of words
penultimateWord :: [String] -> String
penultimateWord s
	| length s == 0	= []
	| length s <= 2	= head s
	| otherwise		= head . tail $ reverse s

-- yields list of penultimate words from list of strings
penultimateWordsList :: [String] -> [String]
penultimateWordsList ss = map penultimateWord $ map words ss
