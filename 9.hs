{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Text.ParserCombinators.Parsec

main :: IO ()
main = do
  s <- removeGarbage <$> readFile "9.txt"
  print s

removeGarbage :: String -> String
removeGarbage [] = []
removeGarbage ('<':xs) =
  removeGarbage (stripGarbage xs)
removeGarbage ('>':xs) =
  removeGarbage xs
removeGarbage (x:xs) =
  x:removeGarbage xs

stripGarbage :: String -> String
stripGarbage ('>':xs) = '>':xs
stripGarbage ('!':xs) =
  stripGarbage (drop 1 xs)
stripGarbage (x:xs) =
  stripGarbage xs

m = (removeGarbage =<< [ "<>"
                       , "<random characters>"
                       , "<<<<>"
                       , "<{!>}>"
                       , "<!!>"
                       , "<!!!>>"
                       , "<{o\"i!a,<{i<a>"
                       ]) == mempty

-- k p s = print $ parse p "foo" s
