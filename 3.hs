{-# LANGUAGE FlexibleContexts #-}
{-#LANGUAGE BangPatterns#-}
module Main where

import           Data.Map        (Map)
import qualified Data.Map.Strict as M
import           Debug.Trace

main :: IO ()
main = print partA

squares :: [[Int]]
squares = [1] : lists genCorner
  where
    lists [] = []
    lists [x] = [[x]]
    lists (x:y:xs) = drop 1 [x..y] : lists (y:xs)
    genCorner = map ((+1) . (*8)) (go 0 1)
      where
        go !n _ | n >= 289326 = []
        go !n !k = n : go (n + k) (k + 1)

data Dir = U | L | R | D
  deriving (Show, Enum)

partA :: Int
partA = abs x + abs y
  where
   (x,y) = resultMap M.! 289326

flippedMap :: Map (Int, Int) Int
flippedMap =
  M.fromList [ (v,k)
             | (k,v) <- M.assocs resultMap
             ]

resultMap :: Map Int (Int, Int)
resultMap = go squares (0,0,1,M.singleton 1 (0,0))
  where
    go [] (_,_,_,map') = map'
    go (xs:xxs) rs =
      go xxs (calcMoves xs rs)

    calcMoves xs rs@(x,y,k,_) = do
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
    shiftCoords (!x,!y,!k,!map') ((!n,!dir):xs) = do
      shiftCoords ( newX
                  , newY
                  , n
                  , M.insert n (newX, newY) map'
                  ) xs
        where
         (!newX, !newY) =
           case dir of
             U -> (x,y+1)
             D -> (x,y-1)
             L -> (x-1,y)
             R -> (x+1,y)

partB = undefined


