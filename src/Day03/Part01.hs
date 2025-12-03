module Day03.Part01 (solution) where

import Data.Foldable (minimumBy)
import Data.Ord (Down (Down), comparing)

parseLine :: String -> [Int]
parseLine = map (read . (: []))

getMaxJoltage :: [Int] -> Int
getMaxJoltage xs = tens * 10 + ones
  where
    (maxI, tens) = minimumBy (comparing (Down . snd)) $ zip [0 ..] (init xs)
    (_, rest) = splitAt (maxI + 1) xs
    ones = maximum rest

solution :: String -> Int
solution s = sum $ map getMaxJoltage parsed
  where
    parsed = map parseLine $ lines s
