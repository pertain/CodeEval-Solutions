{- sc.hs
 -
 - By William Ersing
 -
 - CodeEval challenge (Moderate): Simple Calculator
 - https://www.codeeval.com/open_challenges/94/
 -
 - THIS CHALLENGE IS IN PROGRESS - NO SUBMISSIONS YET
 -}

import qualified Data.List.Split as LS
import qualified Data.ByteString.Lazy.Char8 as LB

bsToInt :: LB.ByteString -> Int
bsToInt bs = case LB.readInt bs of
    Nothing     -> error "Not an Integer"
    Just (x,_)  -> x

splitPlus :: String -> [String]
splitPlus = LS.split (LS.onSublist " + ")

splitMinus :: String -> [String]
splitMinus = LS.split (LS.onSublist " - ")

--splitNegValues :: Eq a => [a] -> [a] -> [[a]]
splitNegValues :: String -> [String]
splitNegValues = LS.split (LS.keepDelimsL $ LS.onSublist "-")

--splitOneOfKeepDelims :: Eq a => [a] -> [a] -> [[a]]
splitOneOfKeepDelims :: String -> [String]
splitOneOfKeepDelims = LS.split (LS.dropBlanks $ LS.oneOf "()^*/")

splitExpression = splitOneOfKeepDelims "()^*/ "


isNeg :: String -> String
isNeg dash@('-':_) = dash
isNeg (a:b:c) = [b]
    where
        a = ' '
        c = ' '
--isNeg (' ':b:' ') = [b]


--solveExp :: [String] -> Float
solveExp [a," - ",c]    = bsToInt (LB.pack a) - bsToInt (LB.pack c)
