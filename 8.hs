module Main where

import qualified Data.Map        as M

main :: IO ()
main = do
  rs <- lines <$> readFile "8.txt"
  let (partB, partA) = parse rs (populate M.empty rs)
  mapM_ print [ partA, partB ]

-- | Populate empty map w/ registers initialized to 0
populate :: M.Map String Int -> [String] -> M.Map String Int
populate m rs = mconcat (map go rs)
  where
    go :: String -> M.Map String Int
    go x = M.insert (words x !! 0) 0 m

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
          go xs highest (M.alter f regName m)
            where
              highest = max (maximum m) h
              f Nothing = Just 0
              f (Just regValue) = do
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
