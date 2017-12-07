{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Monad
import           Data.List
import           Data.List.Split

-- import qualified Data.IntMap   as M
import qualified Data.Map        as M
import qualified Data.Vector     as V

import           Data.Char
import           Data.Maybe

main :: IO ()
main = do
  rs :: [String] <- lines <$> readFile "7.txt"
  nr <- forM rs $ \r ->
    pure $ case words r of
      [name, weight] -> Entry name (readWeight weight) []
      name:weight:"->":xs ->
        Entry name (readWeight weight) (splitOn "," (concat xs))
  print $ M.lookup "azqje" (mkMap nr)
  print $ M.lookup "rfkvap" (mkMap nr) -- 646 -> 655

--  print $ M.lookup "inwmb" (mkMap nr)
--  print $ M.lookup "xaaqdv" (mkMap nr)

  print (findBottom nr)
  let m  = mkMap nr
  forM_ ["holcy","fwbang","inwmb"] $ \n -> do
    print $ calcWeight m n

  -- forM_ ["zxmsme","nbybi","xaaqdv"] $ \n -> do
  --   print $ calcWeight m n

  -- forM_ (findBottom nr)

-- nzeqmqi (15378, should be, 15369)
-- 1060, rfkvap, should be 1051

-- Just (53046,["ghaxmrh","vqxwlkh","nzeqmqi","lokmiua","znypga","vtpoo"])

-- Just (72,["holcy","fwbang","inwmb"])
-- 145260
-- 145260
-- 145269

-- Just (53046,["ghaxmrh","vqxwlkh","nzeqmqi","lokmiua","znypga","vtpoo"])
-- 15369
-- 15369
-- 15378
-- 15369
-- 15369
-- 15369

-- Just (12216,["nmhmw","pknpuej","rfkvap"])
-- 15369
-- 15369
-- 15378
-- 15369
-- 15369
-- 15369

-- Just (12216,["nmhmw","pknpuej","rfkvap"])
-- 1051
-- 1051
-- 1060

-- Just (655,["zxmsme","nbybi","xaaqdv"])
-- ["azqje"]
-- 1051
-- 1051
-- 1060

calcWeight m name = do
  let Just (weight, kids) = M.lookup name m
  weight + sum (map (calcWeight m) kids)

mkMap :: [Entry] -> M.Map String (Int, [String])
mkMap xs = go mempty xs
  where
    go m [] = m
    go m (Entry{..}:xs) =
      go (M.insert name (weight, kids) m) xs

readWeight :: String -> Int
readWeight = read . drop 1 . reverse . drop 1 . reverse

data Entry = Entry { name :: String
                   , weight :: Int
                   , kids :: [String] }
  deriving (Show)

findBottom :: [Entry] -> [String]
findBottom xs =
  map name $ filter (\x -> name x `notElem` allKids) xs
    where
     allKids :: [String] = concatMap kids xs


