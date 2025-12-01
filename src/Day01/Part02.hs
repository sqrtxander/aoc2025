module Day01.Part02 (solution) where

parseLine :: String -> Int
parseLine ('L' : i) = -read i
parseLine ('R' : i) = read i
parseLine _ = error "Bad input."

solution :: String -> Int
solution s =
    sum . map (abs . fst) $
        scanl
            (\(_, acc) x -> (acc + x) `divMod` 100)
            (0, 50)
            parsed
  where
    parsed = map parseLine $ lines s
