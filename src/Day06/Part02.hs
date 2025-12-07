module Day06.Part02 (solution) where

import Data.Char (isSpace)
import Data.List (transpose)
import Data.List.Split (splitOn)

parseOp :: (Foldable t, Num a) => Char -> t a -> a
parseOp '+' = sum
parseOp '*' = product
parseOp _ = error "Bad input"

solution :: String -> Int
solution s = sum $ zipWith (\op xs -> op xs) ops nums
  where
    nums :: [[Int]]
    nums =
        map (map read)
            . splitOn [""]
            . map (filter (not . isSpace))
            . transpose
            . init
            $ lines s
    ops :: [[Int] -> Int]
    ops = map (parseOp . head) . words . last $ lines s
