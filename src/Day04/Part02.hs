module Day04.Part02 (solution) where

import Data.Set (Set)
import qualified Data.Set as S

type Coord = (Int, Int)

parseInput :: String -> Set Coord
parseInput s = S.fromList papers
  where
    papers =
        concatMap
            (\(y, r) -> [(x, y) | (x, c) <- zip [0 ..] r, c == '@'])
            . zip [0 ..]
            $ lines s

neighbors :: Coord -> [Coord]
neighbors (x, y) =
    [ (x - 1, y - 1)
    , (x - 1, y)
    , (x - 1, y + 1)
    , (x, y - 1)
    , (x, y + 1)
    , (x + 1, y - 1)
    , (x + 1, y)
    , (x + 1, y + 1)
    ]

isAccessible :: Set Coord -> Coord -> Bool
isAccessible papers pos =
    length
        ( filter id $
            map
                (`S.member` papers)
                (neighbors pos)
        )
        < 4

simulate :: Set Coord -> Int
simulate papers
    | S.null removed = 0
    | otherwise = S.size removed + simulate papers'
  where
    removed = S.filter (isAccessible papers) papers
    papers' = papers S.\\ removed

solution :: String -> Int
solution s = simulate parsed
  where
    parsed = parseInput s
