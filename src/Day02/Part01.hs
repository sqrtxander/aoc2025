module Day02.Part01 (solution) where

import Data.List.Split (splitOn)

parseRange :: String -> [Int]
parseRange s =
    let ran = splitOn "-" s
     in [read $ head ran .. read $ last ran]

isInvalid :: String -> Bool
isInvalid istr
    | odd $ length istr = False
    | otherwise =
        let len = length istr
            (hi, lo) = splitAt (len `div` 2) istr
         in hi == lo

solution :: String -> Int
solution s = (sum . map read) (concatMap (filter isInvalid . map show) parsed)
  where
    parsed = map parseRange $ splitOn "," s
