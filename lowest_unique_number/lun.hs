import System.Environment (getArgs)
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.List as DL

main :: IO ()
main = do
    inFile <- getArgs
    file <- readFile $ head inFile
    let linesList = lines file
    mapM_ putStrLn linesList    -- placeholder

-- convert ByteString to Int (if valid)
bsToInt :: LB.ByteString -> Int
bsToInt bs = case LB.readInt bs of
    Nothing     -> error "Not an Integer"
    Just (x,_)  -> x

-- convert string (of int chars) to list of Ints
stringsToInts :: String -> [Int]
stringsToInts = map (bsToInt . LB.pack) . words

indexOfLUN lun lst = case DL.elemIndex lun lst of
    Nothing     -> error "No such element"
    Just x      -> x + 1

getUniques :: Ord a => [a] -> [a]
getUniques = map head . filter (\l -> length l == 1) . DL.group . DL.sort

getLUN :: Ord c => [c] -> c
getLUN = minimum . getUniques
