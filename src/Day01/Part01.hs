module Day01.Part01 (solution) where

parseLine :: String -> Int
parseLine ('L' : i) = -read i
parseLine ('R' : i) = read i
parseLine _ = error "Bad input."

solution :: String -> Int
solution s = length . filter (== 0) $ scanl (\acc x -> (acc + x) `mod` 100) 50 parsed
  where
    parsed = map parseLine $ lines s
