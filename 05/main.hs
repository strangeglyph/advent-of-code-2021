{-# LANGUAGE ViewPatterns, DuplicateRecordFields #-}

import System.IO
import Control.Monad
import Data.List
import Data.Maybe
import Debug.Trace

type Point = (Int, Int)
type Input = [(Point, Point)]

main = do
  input <- parse <$> readFile "input"
  putStrLn (show $ solve1 input)
  putStrLn (show $ solve2 input)

parse :: String -> Input
parse = map ventLine . lines

ventLine :: String -> (Point, Point)
ventLine txt = let [start, _, end] = split ' ' txt in (point start, point end)
  where
    point txt = let [x, y] = split ',' txt in (read x, read y)

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

isOrthogonal :: Point -> Point -> Bool
isOrthogonal (x, y) (x1, y1) = (x == x1) || (y == y1)

generatePoints :: Point -> Point -> [Point]
generatePoints (x1, y1) (x2, y2)
  | x1 == x2 && y1 <= y2   = map (injL x1) [y1..y2]
  | x1 == x2 && y2 <= y1   = map (injL x1) [y2..y1]
  | x1 <= x2 && y1 == y2   = map (injR y1) [x1..x2]
  | x2 <= x1 && y1 == y2   = map (injR y1) [x2..x1]
  | x1 <  x2 && y1 <  y2   = zip [x1..x2] [y1..y2]
  | x2 <  x1 && y1 <  y2   = zip (reverse [x2..x1]) [y1..y2]
  | x1 <  x2 && y2 <  y1   = zip [x1..x2] (reverse [y2..y1])
  | otherwise              = zip [x2..x1] [y2..y1]


solve1 :: Input -> Int
solve1 = length . filter (> 1) . map length . group . sort . concatMap (uncurry generatePoints) . filter (uncurry isOrthogonal)

solve2 :: Input -> Int
solve2 = length . filter (> 1) . map length . group . sort . concatMap (uncurry generatePoints)
