module Main where

import Control.Monad
import Data.List

main = partA >> partB

partA :: IO ()
partA = do
  xs <- lines <$> readFile "4.txt"
  print =<< length <$> do
    flip filterM xs $ \line -> do
      let ks = words line
      return $ nub ks == ks

partB :: IO ()
partB = do
  xs <- lines <$> readFile "4.txt"
  zs <- forM xs $ \x -> do
      let ks = words x
      return $ length (nub (map sort ks)) == length ks
  print $ length (filter (==True) zs)







