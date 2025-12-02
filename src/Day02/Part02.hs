module Day02.Part02 (solution) where

import Data.List.Split (splitOn)

parseRange :: String -> [Int]
parseRange s =
    let ran = splitOn "-" s
     in [read $ head ran .. read $ last ran]

numLength :: Int -> Int
numLength 0 = 1
numLength n = length . takeWhile (> 0) $ iterate (`div` 10) (abs n)

isInvalid :: Int -> Bool
isInvalid num = go 1 num
  where
    len = numLength num
    go chunkLen i
        | chunkLen * 2 > len = False
        | len `mod` chunkLen /= 0 = go (chunkLen + 1) i
        | otherwise = allTheSame (numChunkify chunkLen i) || go (chunkLen + 1) i
    allTheSame [] = True
    allTheSame (x : xs) = all (== x) xs
    numChunkify _ 0 = []
    numChunkify chunkLen i =
        let (d, m) = i `divMod` (10 ^ chunkLen)
         in (m : numChunkify chunkLen d)

solution :: String -> Int
solution s = sum $ concatMap (filter isInvalid) parsed
  where
    parsed = map parseRange $ splitOn "," s
