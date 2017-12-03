module Three where

import           Data.Map    (Map)
import qualified Data.Map    as M

squares :: [[Int]]
squares = [1] : lists genCorner
  where
    lists [] = []
    lists [x] = [[x]]
    lists (x:y:xs) = drop 1 [x..y] : lists (y:xs)
    genCorner = map ((+1) . (*8)) (go 0 1)
      where
        go n _ | n >= 289326 = []
        go n k = n : go (n + k) (k + 1)

data Dir = U | L | R | D
  deriving (Show, Enum)

partA :: Int
partA = abs x + abs y + 1
  where
   (x,y) = resultMap M.! 289326

resultMap :: Map Int (Int, Int)
resultMap = go squares (0,0,1,mempty)
  where
    go [] (_,_,_,map') = map'
    go (xs:xxs) rs =
      go xxs (calcMoves xs rs)

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
    shiftCoords (x,y,k,map') ((n,dir):xs) = do
      shiftCoords ( newX
                  , newY
                  , newN
                  , M.insert newN (newX,newY) map'
                  ) xs
        where
         (newX, newY, newN) =
           case dir of
             U -> (x,y+1,n+1)
             D -> (x,y-1,n+1)
             L -> (x-1,y,n+1)
             R -> (x+1,y,n+1)
