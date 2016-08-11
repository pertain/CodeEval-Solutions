import System.Environment (getArgs)

type Row = [Int]

main :: IO ()
main = do
    inFile <- getArgs
    file <- readFile $ head inFile
    let linesList = lines file
    let ((i,pathVal):_) = getPath linesList
    print pathVal
    --mapM_ putStrLn $ linesList

-------------------------------------------

-- Non-monadic pass triangle challange
mir :: Row -> Int -> (Int, Int)
mir [] _    = (0, 0)
mir [x] _   = (0, x)

mir r i
    | x == max x y  = (i,x)
    | otherwise     = (i+1,y)
    where
        (x:y:_) = drop i r

getPath :: [String] -> [(Int,Int)]
getPath = foldl (\acc rw -> let
                                row         = map read $ words rw
                                ((i,t):_)   = acc
                                (i',t')     = mir row i
                            in
                                (i',t+t') : acc
                ) startVal

--getPathVal :: [(Int,Int)] -> Int
--getPathVal ((i,t):_) = 

startVal :: [(Int, Int)]
startVal = [(0, 0)]

-------------------------------------------

--startVal :: (Int, Int)
--startVal = (0, 0)
