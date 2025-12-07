module Day06.Part01 (solution) where

import Data.List (transpose)

parseOp :: (Foldable t, Num a) => Char -> t a -> a
parseOp '+' = sum
parseOp '*' = product
parseOp _ = error "Bad input"

solution :: String -> Int
solution s = sum $ zipWith (\op xs -> op xs) ops nums
  where
    nums :: [[Int]]
    nums = transpose . map (map read . words) . init $ lines s
    ops :: [[Int] -> Int]
    ops = map (parseOp . head) . words . last $ lines s
