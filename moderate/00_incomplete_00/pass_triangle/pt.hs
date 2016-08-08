import Control.Monad.State

type Row = [Int]
type PathValue = [Int]
type PathState = (Int,[Int])

mir :: Row -> Int -> (Int,Int)
mir [] _    = (0,0)
mir [x] _   = (x,0)

mir [x,y] i
    | x == max x y  = (x,i)
    | otherwise     = (y,i+1)

mir r i
    | x' == max x' y'   = (x',i)
    | otherwise         = (y',i+1)
    where
        (x':y':_)   = drop i r

-------------------------------------------
{- Calculates correct state/value/index for a single row,
 - and can be used to chain several rows for a final result
 - (i.e. runState ((smir [5]) >> (smir [9,6])) (0,[])),
 - but needs modification to handle a list of rows recursively
 - (like playGame example)
 -}

smir :: Row -> State PathState PathValue
--smir :: Row -> State (Int,[Int]) [Int]
smir []     = do
    (_,path) <- get
    return path

smir [x]    = do
    (_,path) <- get
    put (0,x:path)
    return (x:path)

smir [x,y]  = do
    (index,path) <- get
    case max x y of
        x   -> (put (index,x:path)) >> return (x:path)
        y   -> (put (index+1,y:path)) >> return (y:path)

smir r      = do
    (index,path) <- get
    let (x:y:_) = drop index r
    case max x y of
        x  -> (put (index+1,x:path)) >> return (x:path)
        y  -> (put (index+2,y:path)) >> return (y:path)
    
startState :: PathState
startState = (0,[])

-------------------------------------------
