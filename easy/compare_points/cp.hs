import System.Environment
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.List.Split (splitEvery, splitPlaces)

bsToInt :: LB.ByteString -> Int
bsToInt bs = case LB.readInt bs of
    Nothing     -> error "Invalid Integer"
    Just (x,_)  -> x

stringToInts :: String -> [[Int]]
--stringToInts s = splitEvery 2 (map (bsToInt . LB.pack) (words s))
stringToInts s = splitPlaces [2,2] (map (bsToInt . LB.pack) (words s))
