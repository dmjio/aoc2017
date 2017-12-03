{-# LANGUAGE FlexibleContexts #-}
{-#LANGUAGE BangPatterns#-}
module Main where

import           Control.Monad
import           Data.Map        (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Debug.Trace

main :: IO ()
main = print partA >> print partB

squares :: [[Int]]
squares = init $ [1] : lists genCorner
  where
    lists [] = []
    lists [x] = [[x]]
    lists (x:y:xs) = drop 1 [x..y] : lists (y:xs)
    genCorner = map ((+1).(*8)) (go 0 1)
      where
        go !n _ | n >= 289326 = []
        go !n !k = n : go (n + k) (k + 1)

data Dir = U | L | R | D
  deriving (Show, Enum)

partA :: Int
partA = abs x + abs y
  where
   (x,y) = fst results

partB :: Int
partB = snd results

results :: ((Int, Int), Int)
results = go squares ( 0
                     , 0
                     , 1
                     , M.singleton 1 (0,0)
                     , M.singleton (0,0) 1
                     , 0
                     )
  where
    go [] (_,_,_,map',_,!x) = (map' M.! 289326, x)
    go (xs:xxs) rs = go xxs $! calcMoves xs rs

    calcMoves xs rs = do
      let moveCount = length xs `div` 4
          moveRight = [ R | moveCount /= 0 ]
          moveDirs  =
            concat [ moveRight
                   , replicate (moveCount - 1) U
                   , replicate moveCount L
                   , replicate moveCount D
                   , replicate moveCount R
                   ]
          moves = zip xs moveDirs
      shiftCoords rs moves

    shiftCoords rs [] = rs
    shiftCoords (!x,!y,_,!map',!sumMap,!max') ((!n,!dir):xs) = do
      let (newMax, newSumMap) = insertSum (newX, newY) n sumMap
      shiftCoords ( newX
                  , newY
                  , n
                  , M.insert n (newX, newY) map'
                  , newSumMap
                  , if max' >= 289326 then max' else newMax
                  ) xs
        where
          (!newX, !newY) =
            case dir of
              U -> (x,y+1)
              D -> (x,y-1)
              L -> (x-1,y)
              R -> (x+1,y)

    insertSum (x,y) n map' = do
      let sum = getAdjSum (x,y) map'
      (sum, M.insert (x,y) sum map')

    getAdjSum (x,y) map' = sum (catMaybes ls)
      where
        ls = [
             M.lookup (x+1,y) map'
           , M.lookup (x,y+1) map'
           , M.lookup (x+1,y+1) map'

           , M.lookup (x-1,y) map'
           , M.lookup (x,y-1) map'
           , M.lookup (x-1,y-1) map'

           , M.lookup (x+1,y-1) map'
           , M.lookup (x-1,y+1) map'
           ]


