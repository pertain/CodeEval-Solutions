{- en.hs
 -
 - By William Ersing
 -
 - CodeEval challenge (EASY) Even Numbers
 - https://www.codeeval.com/open_challenges/100/
 -}

import System.Environment (getArgs)
import qualified Data.ByteString.Lazy.Char8 as LB

main :: IO ()
main = do
    inFile <- getArgs
    file <- readFile $ head inFile
    let linesList = lines file
    mapM_ putStrLn $ isEven linesList

bsToInt :: LB.ByteString -> Int
bsToInt bs = case LB.readInt bs of
    Nothing     -> error "Invalid Integer"
    Just (x,_)  -> x

boolToStr :: Bool -> String
boolToStr b
    | b         = "1"
    | otherwise = "0"

isEven :: [String] -> [String]
isEven = map (boolToStr . even . bsToInt . LB.pack)
