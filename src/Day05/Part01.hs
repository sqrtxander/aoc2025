module Day05.Part01 (solution) where

import Data.List (sortBy)
import Data.List.Split (splitOn)
import Data.Ord (comparing)

parseInput :: String -> ([(Int, Int)], [Int])
parseInput s = (ranges', ingredients')
  where
    parseRange :: String -> (Int, Int)
    parseRange s' =
        let ran = splitOn "-" s'
         in (read $ head ran, read $ last ran)
    (ranges, ingredients) = case splitOn "\n\n" s of
        [r, i] -> (r, i)
        _ -> error "Bad input"
    ranges' = map parseRange $ lines ranges
    ingredients' = map read $ lines ingredients

simplifyRanges :: [(Int, Int)] -> [(Int, Int)]
simplifyRanges [] = []
simplifyRanges ranges = go [] $ sortBy (comparing fst) ranges
    where
    go :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
    go [] (r : rs) = go [r] rs
    go acc [] = acc
    go ((lo, hi) : acc) ((lo', hi') : rs)
        | lo' <= hi + 1 = go ((lo, max hi hi') : acc) rs
        | otherwise = go ((lo', hi') : (lo, hi) : acc) rs

isFresh :: [(Int, Int)] -> Int -> Bool
isFresh ranges ingredient = any (\(lo, hi) -> ingredient >= lo && ingredient <= hi) ranges

solution :: String -> Int
solution s = length $ filter (isFresh ranges') ingredients
  where
    (ranges, ingredients) = parseInput s
    ranges' = simplifyRanges ranges
