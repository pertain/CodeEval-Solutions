{- mtl.hs
 -
 - By William Ersing
 -
 - This is a programming challenge from CodeEval.com.
 - It reads in a file where each line contains a
 - space-separated list of characters, then an integer.
 -
 - Example:		a b c d 4
 -
 - The terminating integer represents the index of the
 - character to be extracted from the end of the list
 - (mth from last element (1-base)).
 -
 - From the example above:
 -
 - 	index:	4
 - 	list:	a b c d
 - 	
 - 	mthFromLast: a
 -}

module Main where
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy.Char8 as LB

main :: IO ()
main = do
	[inFile] <- getArgs
	file <- readFile inFile
	let linesList = lines file
	mapM_ putStrLn $ mthList linesList

-- convert ByteString to Int (if valid)
bsToInt :: LB.ByteString -> Int
bsToInt bs = case LB.readInt bs of
	Nothing -> error "Not an Integer"
	Just (x,_) -> x

-- parses out the given index (last element in each string)
lineIndex :: String -> Int
lineIndex s = bsToInt . LB.pack . last $ words s

-- yields the given list sans index (everything except last element)
sansIndex :: String -> [String]
sansIndex s = init $ words s

-- determine if the given index is valid for the given list
validIndex :: String -> Bool
validIndex s
	| a > 0 && a <= (length $ sansIndex s) = True
	| otherwise = False
	where
		a = lineIndex s

-- yields mth to last element in a given list
mthFromLast :: String -> String
mthFromLast s = (reverse $ sansIndex s) !! ((lineIndex s) - 1)

-- yields list of mthFromLast elements
mthList :: [String] -> [String]
mthList ss = foldr (\x acc -> if validIndex x then mthFromLast x : acc else acc) [] ss
