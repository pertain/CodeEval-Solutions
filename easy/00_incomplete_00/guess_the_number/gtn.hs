{- gtn.hs
 -
 - By William Ersing
 -
 - CodeEval challenge (MODERATE) Guess the Number
 - https://www.codeeval.com/open_challenges/170/
 -}

import System.Environment (getArgs)
import qualified Data.ByteString.Lazy.Char8 as LB

main :: IO ()
main = do
    inFile <- getArgs
    file <- readFile $ head inFile
    let linesList = lines file
    mapM_ putStrLn $ parseAllLines linesList

bsToInt :: LB.ByteString -> Int
bsToInt bs = case LB.readInt bs of
    Nothing     -> error "Invalid Integer"
    Just (x,_)  -> x

getMid :: Int -> Int -> Int
getMid lo hi
    | evenLen   = evenDist
    | otherwise = oddDist
    where
        len         = length [lo .. hi]
        evenLen     = even len
        distance    = (hi - lo) `div` 2
        evenDist    = lo + distance + 1
        oddDist     = lo + distance

getAllMids :: [String] -> Int -> Int -> [(Int,Int)]
getAllMids hiLoLst initLo initHi
    = foldr (\x acc ->
                let oldLo   = fst $ head acc
                    oldHi   = snd $ head acc
                    olderHi = snd $ head $ tail acc
                in
                    if x == "Lower"
                    then (oldLo, getMid oldLo (oldHi - 1)) : acc
                    else (oldHi + 1, getMid oldHi (olderHi + 1)) : acc
                    ) [(initLo, initHi)] hiLoLst

getFinalNumLine :: [(Int,Int)] -> String
getFinalNumLine = show . snd . head

parseLine :: String -> String
parseLine ls = getFinalNumLine $ getAllMids hiLoString floor roof
    where
        lineData    = init $ words ls
        hiLoString  = reverse $ "Lower" : (tail lineData)
        floor       = 0
        roof        = bsToInt $ LB.pack $ head lineData

parseAllLines :: [String] -> [String]
parseAllLines = map parseLine
