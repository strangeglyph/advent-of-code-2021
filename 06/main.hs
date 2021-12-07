{-# LANGUAGE ViewPatterns, DuplicateRecordFields #-}

import System.IO
import Control.Monad
import Data.List
import Data.Maybe
import Debug.Trace
import GHC.Exts (groupWith)

type Input = [Int]

main = do
  input <- parse <$> readFile "input"
  putStrLn (show $ solve1 input)
  putStrLn (show $ solve2 input)

parse :: String -> Input
parse = map read . split ','


injL :: a -> b -> (a, b)
injL a b = (a, b)

injR :: a -> b -> (b, a)
injR a b = (b, a)

split :: (Eq a) => a -> [a] -> [[a]]
split _ [] = []
split sep (x:xs)
  | sep == x  = [] : split sep xs
  | otherwise = prepend x (split sep xs)
  where
    prepend x [] = [[x]]
    prepend x (y:ys) = (x:y):ys

subst :: (Eq a) => a -> a -> a -> a
subst tgt repl val
  | val == tgt  = repl
  | otherwise   = val

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

fork :: (a -> b) -> (a -> c) -> a -> (b, c)
fork f g x = (f x, g x)

mapp :: (a -> b) -> [[a]] -> [[b]]
mapp f = map (map f)

groupByAge :: [Int] -> [(Int, Integer)]
groupByAge = map (\xs -> (head xs, toInteger $ length xs)) . group . sort

updateAgeGroup :: Int -> Integer -> [(Int, Integer)]
updateAgeGroup 0 n = [(6, n), (8, n)]
updateAgeGroup age n = [(age - 1, n)]

groupByAge' :: [(Int, Integer)] -> [(Int, Integer)]
groupByAge' = map (fork (head . fst) (sum . snd) . unzip) . groupWith fst . sort


generate :: Input -> [[(Int, Integer)]]
generate = unfoldr (Just . fork id (groupByAge' . concatMap (uncurry updateAgeGroup))) . groupByAge

solve1 :: Input -> Integer
solve1 inp = sum $ map snd $ ((generate inp) !! 80)

solve2 :: Input -> Integer
solve2 inp = sum $ map snd $ ((generate inp) !! 256)
