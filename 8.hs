{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Monad
import           Data.Char
import           Data.Function
import           Data.List
import           Data.List.Split
import qualified Data.Map        as M
import           Data.Maybe
import           Data.Set
import           Data.Tree
import qualified Data.Vector     as V

ks =
  ["b inc 5 if a > 1"
  ,"a inc 1 if b < 5"
  ,"c dec -10 if a >= 1"
  ,"c inc -20 if c == 10"
  ]

main :: IO ()
main = do
  rs :: [String] <- lines <$> readFile "8.txt"
  let (max', m) = parse (populate M.empty rs) rs
  print $ snd $ head $ sortBy (flip compare `on` snd) $ M.toList m
  print max'

populate :: M.Map String Int -> [String] -> M.Map String Int
populate m rs = mconcat $ Prelude.map go rs
  where
    go :: String -> M.Map String Int
    go x = M.insert (words x !! 4) 0 (M.insert (words x !! 0) 0 m)

parse :: M.Map String Int -> [String] -> (Int, M.Map String Int)
parse m' rs = go m' 0 rs
  where
    go :: M.Map String Int -> Int -> [String] -> (Int, M.Map String Int)
    go m h [] = (h,m)
    go m h' (x:xs) =
      case words x of
        regName:op':val:"if":regName2:cond:pred:_ -> go (M.alter f regName m) h xs
            where
              h = if h' > highest
                    then h'
                    else highest
              highest = snd $ head $ sortBy (flip compare `on` snd) $ M.toList m
              f Nothing = error "hmm"
              f (Just regValue) = do
                let r' = m M.! regName2
                    zz = (toPred cond) r' (read pred :: Int)
                if zz
                   then do
                      pure $ case op' of
                        "inc" -> regValue + read val
                        "dec" -> regValue - read val
                   else Just regValue

toPred "<" = (<)
toPred ">" = (>)
toPred ">=" = (>=)
toPred "<=" = (<=)
toPred "!=" = (/=)
toPred "==" = (==)
