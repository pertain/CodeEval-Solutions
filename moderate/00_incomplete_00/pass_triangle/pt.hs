import System.Environment (getArgs)

type Row = [Int]

main :: IO ()
main = do
    inFile <- getArgs
    file <- readFile $ head inFile
    let linesList = lines file
    mapM_ putStrLn $ linesList

-- Non-monadic pass triangle challange
mir :: Row -> Int -> (Int, Int)
mir [] _    = (0, 0)
mir [x] _   = (x, 0)

mir [x, y] i
    | x == max x y  = (x, i)
    | otherwise     = (y, i + 1)

mir r i
    | x' == max x' y'   = (x', i)
    | otherwise         = (y', i + 1)
    where
        (x':y':_)   = drop i r

startState :: (Int, Int)
startState = (0, 0)
