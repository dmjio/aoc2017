{-# LANGUAGE ScopedTypeVariables #-}

main = print =<< two

two :: IO Int
two =
    sum .
    map go .
    fmap (map read) .
    map words .
    lines <$> readFile "2.txt"

go :: [Int] -> Int
go xs = maximum xs - minimum xs
