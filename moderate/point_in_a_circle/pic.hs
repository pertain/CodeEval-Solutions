{- pic.hs
 -
 - By William Ersing
 -
 - CodeEval challenge (MODERATE) Point in Circle
 - https://www.codeeval.com/open_challenges/98/
 -}

import System.Environment (getArgs)

data Circle = Circle {center :: Point, radius :: Double} deriving (Show)
type Point  = (Double,Double)

main = do
    inFile <- getArgs
    file <- readFile $ head inFile
    let linesList = lines file
    mapM_ putStrLn $ checkAllCircles $ linesList

strToDbl :: String -> Double
strToDbl s
    | notDbl    = error "Invalid Double"
    | otherwise = floatVal
    where
        pair        = reads s :: [(Double, String)]
        notDbl      = length pair == 0
        floatVal    = fst $ head pair

{-
decPrecision :: Double -> Int -> Double
decPrecision x n = (fromIntegral $ round (x * digs)) / digs
    where
        digs = 10^n
-}

splitLn :: (Char -> Bool) -> String -> [String]
splitLn del []  = []
splitLn del s   = w : splitLn del (drop 2 rest)
    where
        (w,rest) = span del s

parseLine :: String -> (Circle,Point)
parseLine s = (circ,pt)
    where
        parts@(c:r:p:_) = splitLn (/=';') s
        circ            = (Circle (parsePoint c) (parseRad r))
        pt              = parsePoint p

parsePoint :: String -> Point
parsePoint s = (x,y)
    where
        stripped    = (init . tail) $ dropWhile (/= '(') s
        pointList   = splitLn (/= ',') stripped
        x           = strToDbl $ head pointList
        y           = strToDbl $ last pointList

parseRad :: String -> Double
parseRad = strToDbl . last . words

pointInCircle :: (Circle,Point) -> String
pointInCircle ((Circle (x1,y1) r),(x2,y2))
    | dist < r = "true"
    | otherwise = "false"
    where
        xs      = (x2 - x1)^2
        ys      = (y2 - y1)^2
        dist    = sqrt (xs + ys)
        --dist    = decPrecision (sqrt (xs + ys)) 3

checkAllCircles :: [String] -> [String]
checkAllCircles = map (pointInCircle . parseLine)
