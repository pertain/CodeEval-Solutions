import System.Environment (getArgs)
import Control.Monad.State

type Row = [Int]

main :: IO ()
main = do
    [inFile] <- getArgs
    file <- readFile inFile
    let linesList = lines file
    print $ evalState (getPath linesList) startState

-------------------------------------------

{- This version keeps a list of values selected from every row
type PathValue = [Int]
type PathState = (Int, [Int])

-- getRowVal updates the state with a new index-rowValue pair
getRowVal :: Row -> State PathState ()
getRowVal []     = do
    (_, _) <- get
    put (0, [])

getRowVal [x]    = do
    (_, total) <- get
    put (0, x : total)

getRowVal r      = do
    (index, total) <- get
    let (x:y:_) = drop index r
    case x == max x y of
        True    -> put (index, x : total)
        False   -> put (index + 1, y : total)

-- getPath recursively calls getRowVal on a list of rows
getPath :: [String] -> State PathState PathValue
getPath []      = do
    (_, total) <- get
    return total

getPath (r:rs)  = do
    let row = map read $ words r
    getRowVal row 
    getPath rs
    
startState :: PathState
startState = (0, [])
-}

-------------------------------------------

--{- This version keeps a cumulative sum of the values selected from every row
type PathValue = Int
type PathState = (Int, Int)

-- getRowVal updates the state with a new index-rowValue pair
getRowVal :: Row -> State PathState ()
getRowVal []     = do
    (_, _) <- get
    put (0, 0)

getRowVal [x]    = do
    (_, total) <- get
    put (0, total + x)

getRowVal r      = do
    (index, total) <- get
    let (x:y:_) = drop index r
    case x == max x y of
        True    -> put (index, total + x)
        False   -> put (index + 1, total + y)

-- getPath recursively calls getRowVal on a list of rows
getPath :: [String] -> State PathState PathValue
getPath []      = do
    (_, total) <- get
    return total

getPath (r:rs)  = do
    let row = map read $ words r
    getRowVal row 
    getPath rs
    
startState :: PathState
startState = (0, 0)
---}

-------------------------------------------
