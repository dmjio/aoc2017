{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Bool
import           Control.Lens
import           Control.Monad
import           Control.Monad.State
import           Data.List
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as MV

main :: IO ()
main = do
  ls :: [String] <- lines <$> readFile "5.txt"
  let nums :: [Int] = map read ls
  partA nums >> partB nums

partA :: [Int] -> IO ()
partA nums =
    flip evalStateT (nums, 0, 0) $ fix $ \loop -> do
      (nums, !index, !moveCount) <- get
      let n = length nums
          k = nums !! index
          newIndex = k + index
          !newNums = nums & ix index .~ k + 1
      if newIndex >= n
         then liftIO $ print (moveCount + 1)
         else do
           put (newNums, newIndex, moveCount + 1)
           loop

partB :: [Int] -> IO ()
partB nums = do
  let vec = V.fromList nums
  go 0 0 (V.length vec) =<< V.thaw vec
    where
      go !index !moveCount !n m =
         if index >= n
            then
              print moveCount
            else do
               k <- MV.unsafeRead m index
               bool minus add (k>=3)
               go (k + index) (moveCount + 1) n m
                 where
                   add = MV.unsafeModify m (subtract 1) index
                   minus = MV.unsafeModify m (+1) index
