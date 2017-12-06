{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns        #-}
module Main where

import qualified Data.Map            as M
import qualified Data.Vector         as V

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
  v <- V.fromList . map read . words <$> readFile "6.txt"
  print (findSolution v)

findSolution :: V.Vector Int -> (Integer, Integer)
findSolution = go mempty 0
    where
      go m !n rs' =
        case M.lookup rs' m of
          Just found -> (n, n - found)
          Nothing ->
            go (M.insert rs' n m) (n + 1) (step rs')

-- unit tests
z = step (V.fromList [0,2,7,0]) == V.fromList [2,4,1,2]
z' = step (V.fromList [2,4,1,2]) == V.fromList [3,1,2,3]
z'' = step (V.fromList [3,1,2,3]) == V.fromList [0,2,3,4]
z''' = step (V.fromList [0,2,3,4]) == V.fromList [1,3,4,1]
tests = and [z,z',z'',z''']
