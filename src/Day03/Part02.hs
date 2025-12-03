module Day03.Part02 (solution) where

import Data.Foldable (minimumBy)
import Data.Ord (Down (Down), comparing)

parseLine :: String -> [Int]
parseLine = map (read . (: []))

getMaxJoltage :: [Int] -> Int
getMaxJoltage = go 12
  where
    go 0 _ = 0
    go i xs = tens * 10 ^ (i - 1) + go (i - 1) rest
      where
        (start, _) = splitAt (length xs - i + 1) xs
        (maxI, tens) = minimumBy (comparing (Down . snd)) $ zip [0 ..] start
        (_, rest) = splitAt (maxI + 1) xs

solution :: String -> Int
solution s = sum $ map getMaxJoltage parsed
  where
    parsed = map parseLine $ lines s
