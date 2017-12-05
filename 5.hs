{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Lens
import           Control.Monad
import           Control.Monad.State
import           Data.List
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as MV
import           Debug.Trace
import           GHC.ST

main :: IO ()
main = do
  ls :: [String] <- lines <$> readFile "5.txt"
  let nums :: [Int] = map read ls
  partA nums >> partB nums

partA :: [Int] -> IO ()
partA nums =
    flip evalStateT (nums, 0, 0) $ fix $ \loop -> do
      (nums, index, moveCount) <- get
      let n = length nums
          k = nums !! index
          newIndex = k + index
          newNums = nums & ix index .~ k + 1
      if newIndex >= n
         then liftIO (print moveCount)
         else do
           put (newNums, newIndex, moveCount + 1)
           loop

partB :: [Int] -> IO ()
partB nums = do
  let l = V.length (V.fromList nums)
      !_ = V.modify (flip go l) (V.fromList nums)
  pure ()
    where
      go :: V.MVector s Int -> Int -> ST s ()
      go m len = go' 0 0 m len

      go' !index !moveCount m !n = do
         if index >= n
            then do
              traceShow moveCount $ pure ()
            else do
               k <- MV.unsafeRead m index
               let newIndex = k + index
               if k >= 3
                  then do
                    subtract 1 <$> MV.unsafeRead m index >>=
                      MV.unsafeWrite m index
                  else do
                    (+1) <$> MV.unsafeRead m index >>=
                      MV.unsafeWrite m index
               go' newIndex (moveCount + 1) m n
