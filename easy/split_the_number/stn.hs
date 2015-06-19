{- stn.hs
 -
 - By William Ersing
 -
 - CodeEval challenge (EASY) Split the Number
 - https://www.codeeval.com/open_challenges/131/
 -}

import System.Environment (getArgs)
import qualified Data.List.Split as LS
import qualified Data.ByteString.Lazy.Char8 as LB

main :: IO ()
main = do
    inFile <- getArgs
    file <- readFile $ head inFile
    let linesList = lines file
    mapM_ putStrLn $ splitLineAll linesList

bsToInt :: LB.ByteString -> Int
bsToInt bs = case LB.readInt bs of
    Nothing     -> error "Invalid Integer"
    Just (x,_)  -> x

performOps :: String -> Int -> Int -> Int
performOps op l r
    | op == "-" = l - r
    | op == "+" = l + r

splitLine :: String -> Int
splitLine s = performOps op l r
    where
        splt    = words s
        spltR   = LS.split (LS.oneOf "-+") (last splt)
        lenL    = length $ head spltR
        lenR    = length $ last spltR
        spltL   = LS.splitPlaces [lenL,lenR] (head splt)
        l       = bsToInt $ LB.pack $ head spltL
        r       = bsToInt $ LB.pack $ last spltL
        op      = spltR !! 1

splitLineAll :: [String] -> [String]
splitLineAll = map (show . splitLine)
