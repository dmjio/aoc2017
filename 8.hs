module Main where

import           Data.Bool
import qualified Data.Map   as M
import           Data.Maybe

main :: IO ()
main = do
  rs <- lines <$> readFile "8.txt"
  let (partB, partA) = parse rs M.empty
  mapM_ print [ partA, partB ]

-- | Given an initialized map of registers and a list of instructions
-- Find the maximum value in a register after applying all instructions
-- And find the maximum value that was ever recorded in a register (part b)
parse :: [String] -> M.Map String Int -> (Int, Int)
parse rs = go rs 0
  where
    go :: [String]
       -> Int
       -> M.Map String Int
       -> (Int, Int)
    go [] h m = (h, maximum m)
    go (x:xs) h m =
      case words x of
        regName:op:val:"if":regName2:cond:pred:_ ->
          go xs highest (M.alter (f . fromMaybe 0) regName m)
            where
              highest = max safeMax h
              safeMax = bool (maximum m) 0 (M.size m == 0)
              f regValue = do
                let (#%#) = toCond cond
                pure $ if M.findWithDefault 0 regName2 m #%# read pred
                         then
                           case op of
                             "inc" -> regValue + read val
                             "dec" -> regValue - read val
                         else regValue

-- | Create conditional operation
toCond :: (Eq a, Ord a) => String -> (a -> a -> Bool)
toCond "<" = (<)
toCond ">" = (>)
toCond ">=" = (>=)
toCond "<=" = (<=)
toCond "!=" = (/=)
toCond "==" = (==)
