import System.Environment (getArgs)
import Data.List.Split

main :: IO ()
main = do
    inFile <- getArgs
    file <- readFile $ head inFile
    let linesList = lines file
    mapM_ print $ parseAll linesList

--parseLine :: String -> Bool
--parseLine s = rotated == target
parseLine :: String -> (String,String)
parseLine s = (target,rotated)
    where
        splitLine   = splitOn "," s
        scrambled   = last splitLine
        target      = head splitLine
        firstLetter = take 1 target
        splitScram  = split (keepDelimsL $ onSublist firstLetter) scrambled
        --rotated     = last splitScram ++ head splitScram
        rotated     = if length splitScram <= 2
                      then last splitScram ++ head splitScram
                      else last splitScram ++ concat (init splitScram)

--parseAll :: [String] -> [Bool]
--parseAll = map parseLine

--parseAll :: [String] -> [Bool]
parseAll :: [String] -> [(String,String)]
parseAll = map parseLine
