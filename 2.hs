{-# LANGUAGE ScopedTypeVariables #-}

main = print =<< two go

b = print =<< two partB

two k =
    sum .
    map k .
    fmap (map read) .
    map words .
    lines <$> readFile "2.txt"

go :: [Int] -> Int
go xs = maximum xs - minimum xs

partB :: [Int] -> Int
partB xs = head [ x `div` y | x <- xs, y <- xs, x `mod` y == 0, x /= y ]
