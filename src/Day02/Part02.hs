module Day02.Part02 (solution) where

import Data.List.Split (chunksOf, splitOn)

parseRange :: String -> [Int]
parseRange s =
    let ran = splitOn "-" s
     in [read $ head ran .. read $ last ran]

isInvalid :: String -> Bool
isInvalid = go 1
  where
    go :: Int -> String -> Bool
    go len istr
        | len * 2 > length istr = False
        | otherwise =
            let cnks = chunksOf len istr
             in (allTheSame cnks || go (len + 1) istr)
    allTheSame [] = True
    allTheSame (x : xs) = all (== x) xs

solution :: String -> Int
solution s = (sum . map read) (concatMap (filter isInvalid . map show) parsed)
  where
    parsed = map parseRange $ splitOn "," s
