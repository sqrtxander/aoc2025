module Day01.Part02 (solution) where

parseLine :: String -> Int
parseLine ('L' : i) = -read i
parseLine ('R' : i) = read i
parseLine _ = error "Bad input."

countZeros :: (Int, Int) -> Int -> (Int, Int)
countZeros (_, acc) x
    | x <= 0 =
        let flipDial p = (100 - p) `mod` 100
            acc' = flipDial acc
            (d, m) = (acc' - x) `divMod` 100
         in (d, flipDial m)
    | otherwise = (acc + x) `divMod` 100

solution :: String -> String
solution s =
    show . sum . map (abs . fst) $
        -- show $
        scanl
            countZeros
            (0, 50)
            parsed
  where
    parsed = map parseLine $ lines s
