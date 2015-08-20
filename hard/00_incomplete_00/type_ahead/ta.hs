import Data.List (sort, group)

lambString :: String
lambString =   "Mary had a little lamb its fleece was white as snow\
                \ And everywhere that Mary went the lamb was sure to go\
                \ It followed her to school one day which was against\
                \ the rule It made the children laugh and play to see a\
                \ lamb at school And so the teacher turned it out but\
                \ still it lingered near And waited patiently about till\
                \ Mary did appear Why does the lamb love Mary so the\
                \ eager children cry Why Mary loves the lamb you know\
                \ the teacher did reply"

lambWords :: [String]
lambWords = words lambString

nGramsAll :: Int -> [String] -> [[String]]
nGramsAll n ss@(x:xs)
    | len < n   = []
    | len == n  = [ss]
    | len > n   = take n ss : nGramsAll n xs
    where
        len = length ss

nGramsTerm :: String -> [[String]] -> [[String]]
nGramsTerm s =
        filter (\x -> take termLen x == termWords)
    where
        termWords   = words s
        termLen     = length termWords


-- Working toward the probabilities of each word
-- The following part is just a nudge for grouping/sorting
bunches :: String -> [[String]]
bunches = group . sort . words

-- The following is a nudge for getting occurrence counts
occurences :: [[String]] -> [(String,Int)]
occurences = map (\x -> (head x, length x))
--occurences = map head Control.Arrow.&&& length
