{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-#LANGUAGE BangPatterns#-}
module Main where

import           Control.Lens
import           Control.Monad.State
import           Data.Function
import qualified Data.IntMap         as I
import           Data.List
import qualified Data.Map            as M
import qualified Data.Sequence       as S
import qualified Data.Vector         as V
import           Debug.Trace

step :: V.Vector Int -> V.Vector Int
step vec = snd $ iterate go (maxIdx + 1, vec V.// [(maxIdx,0)]) !! maxVal
  where
    maxIdx = V.maxIndex vec
    maxVal = V.maximum vec
    go (idx, vec) = (newIdx + 1, vec V.// xs)
      where
        newIdx = idx `mod` V.length vec
        val = vec V.! newIdx
        xs = [(newIdx, val + 1)]

main :: IO ()
main = do
  rs :: V.Vector Int <- V.fromList . map read . words <$> readFile "6.txt"
  () <- flip evalStateT (0,rs, [rs], False) $ fix $ \loop -> do
    (!seenCount, rs', others, hasSeen) <- get
    let newRs = step rs'
    if newRs `elem` others
      then do
        let Just ixxx' = elemIndex newRs others
        liftIO $ print seenCount
      else do
        if hasSeen
          then do
            put (seenCount + 1, newRs, newRs:others, hasSeen)
          else
            if newRs == V.fromList [10,9,8,7,6,5,4,3,1,1,0,15,14,13,11,12]
            then
              put (seenCount + 1, newRs, newRs:others, True)
            else
              put (seenCount, newRs, newRs:others, hasSeen)
        loop
  print ()

-- unit tests
z = step (V.fromList [0,2,7,0]) == V.fromList [2,4,1,2]
z' = step (V.fromList [2,4,1,2]) == V.fromList [3,1,2,3]
z'' = step (V.fromList [3,1,2,3]) == V.fromList [0,2,3,4]
z''' = step (V.fromList [0,2,3,4]) == V.fromList [1,3,4,1]
tests = and [z,z',z'',z''']
