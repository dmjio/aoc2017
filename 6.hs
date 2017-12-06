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

main :: IO ()
main = do
  rs :: V.Vector Int <- V.fromList . map read . words <$> readFile "6.txt"
  () <- flip evalStateT (0,rs, [rs], False) $ fix $ \loop -> do
    (!seenCount, rs', others, hasSeen) <- get
    let newRs = updateVec rs'
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

updateVec :: V.Vector Int -> V.Vector Int
updateVec xs = do
   let val' = V.maximum xs
       index = V.maxIndex xs
       newRs' :: V.Vector Int = xs & ix index .~ 0
   let (_, kk, _) = flip execState (val', newRs', index + 1) $ fix $ \innerLoop -> do
          (!v, ks, !ix') <- get
          if v == 1
            then do
              let newKs' = ks & ix ix' %~ (+1)
              put (v, newKs', ix' + 1)
            else if ix' >= length ks - 1
                 then do
                    let newKs' = ks & ix ix' %~ (+1)
                    put (if ix' > length ks - 1 then v else v - 1, newKs', 0)
                    innerLoop
                 else do
                    let newKs' = ks & ix ix' %~ (+1)
                    put (v - 1, newKs', ix'+1)
                    innerLoop
   kk

-- unit tests
z = updateVec (V.fromList [0,2,7,0]) == V.fromList [2,4,1,2]
z' = updateVec (V.fromList [2,4,1,2]) == V.fromList [3,1,2,3]
z'' = updateVec (V.fromList [3,1,2,3]) == V.fromList [0,2,3,4]
z''' = updateVec (V.fromList [0,2,3,4]) == V.fromList [1,3,4,1]

kk = and [z,z',z'',z''']
