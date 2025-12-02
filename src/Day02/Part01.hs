module Day02.Part01 (solution) where

import Data.List.Split (splitOn)

parseRange :: String -> [Int]
parseRange s =
    let ran = splitOn "-" s
     in [read $ head ran .. read $ last ran]

numLength :: Int -> Int
numLength 0 = 1
numLength n = length . takeWhile (> 0) $ iterate (`div` 10) (abs n)

isInvalid :: Int -> Bool
isInvalid i
    | odd len = False
    | otherwise =
        let (hi, lo) = i `divMod` (10 ^ (len `div` 2))
         in hi == lo
  where
    len = numLength i

solution :: String -> Int
solution s = sum $ concatMap (filter isInvalid) parsed
  where
    parsed = map parseRange $ splitOn "," s
