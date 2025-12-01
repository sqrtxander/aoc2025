module Day01.Part02 (solution) where

parseLine :: String -> Int
parseLine ('L' : i) = -read i
parseLine ('R' : i) = read i
parseLine _ = error "Bad input."

getZeros :: (Int, Int) -> Int -> (Int, Int)
getZeros (acc, _) x =
    let point = acc + x
     in (point `mod` 100, abs $ point `div` 100)

solution :: String -> Int
solution s = sum . map snd $ scanl getZeros (50, 0) parsed
  where
    parsed = map parseLine $ lines s
