import Data.List.Split

parseLine :: String -> (String,String)
parseLine s = (target,mergedRot)
    where
        splitLine   = splitOn "," s
        scrambled   = last splitLine
        target      = head splitLine
        firstLetter = head target
        mergedRot   = concat $ split (keepDelimsL $ onSublist firstLetter) scrambled
