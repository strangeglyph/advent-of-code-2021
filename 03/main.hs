{-# LANGUAGE ViewPatterns #-}

import System.IO
import Control.Monad
import Data.List
import Data.Char (digitToInt)

main = do
  input <- parse <$> readFile "input"
  putStrLn (show $ solve1 input)
  putStrLn (show $ solve2 input)

parse :: String -> [[Int]]
parse = map (map digitToInt) . lines

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

fromBinary = foldl (\acc bit -> 2 * acc + bit) 0

bitOrder :: [Int] -> (Int, Int)
bitOrder xs = if 2 * (sum xs) >= length xs then (1, 0) else (0, 1)


solve1 :: [[Int]] -> Int
solve1 = uncurry (*) . both fromBinary . unzip . map bitOrder . transpose
  where

solve2 :: [[Int]] -> Int
solve2 xs = let
    oxygenRating = fromBinary $ filterCriterion (fst . bitOrder) xs
    co2Scrubber  = fromBinary $ filterCriterion (snd . bitOrder) xs
  in
    oxygenRating * co2Scrubber
  where
    filterCriterion :: ([Int] -> Int) -> [[Int]] -> [Int]
    filterCriterion criterion [val] = val
    filterCriterion criterion xs = let selected = criterion (head $ transpose xs)
                                       remaining = map tail $ filter (\xs -> head xs == selected) xs
                                   in
                                       (selected:) $ filterCriterion criterion remaining
