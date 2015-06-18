{- cp.hs
 -
 - By William Ersing
 -
 - CodeEval challenge (EASY)
 - https://www.codeeval.com/open_challenges/192/
 -}

import System.Environment
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.List.Split (splitPlaces)

main :: IO ()
main = do
    inFile <- getArgs
    file <- readFile $ head inFile
    let linesList = lines file
    mapM_ putStrLn $ getAllDirections linesList

bsToInt :: LB.ByteString -> Int
bsToInt bs = case LB.readInt bs of
    Nothing     -> error "Invalid Integer"
    Just (x,_)  -> x

intCoords :: String -> [[Int]]
intCoords s = splitPlaces [2,2] (map (bsToInt . LB.pack) (words s))

getDirection :: [[Int]] -> String
getDirection is
    | q == o    &&  r > p   = "N"
    | q > o     &&  r > p   = "NE"
    | q > o     &&  r == p  = "E"
    | q > o     &&  r < p   = "SE"
    | q == o    &&  r < p   = "S"
    | q < o     &&  r < p   = "SW"
    | q < o     &&  r == p  = "W"
    | q < o     &&  r > p   = "NW"
    | q == o    &&  r == p  = "here"
    where
        o = head $ head is
        p = last $ head is
        q = head $ last is
        r = last $ last is

getAllDirections :: [String] -> [String]
getAllDirections = map (getDirection . intCoords)
