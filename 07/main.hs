{-# LANGUAGE ViewPatterns, DuplicateRecordFields #-}

import System.IO
import Control.Monad
import Data.List
import Data.Maybe
import Debug.Trace
import GHC.Exts (groupWith)

type Input = [(Int, Int)]

main = do
  inp <- input
  putStrLn (show $ solve1 inp)
  putStrLn (show $ solve2 inp)

input :: IO Input
input = parse <$> readFile "input"

parse :: String -> Input
parse = map (fork head length) . group . sort . map read . split ','

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

liftTup :: (a -> b) -> (c -> d) -> (a , c) -> (b, d)
liftTup f g (x, y) = (f x, g y)

mapp :: (a -> b) -> [[a]] -> [[b]]
mapp f = map (map f)

pam :: [a -> b] -> a -> [b]
pam [] x = []
pam (f:fs) x = f x : pam fs x

cumr :: Input -> Input
cumr [] = []
cumr [x] = [x]
cumr ((pos, n):xs) = liftTup (const pos) (+n) (head $ cumr xs) : cumr xs

distTriang :: Int -> Input -> Int
distTriang k = sum . map (\(pos, n) -> n * triang (abs $ pos - k))
  where
    triang n = n * (n + 1) `div` 2

solve1 :: Input -> Int
solve1 x = sum $ map (\(pos, n) -> abs (pos - halfway) * n) x
  where
    halfway = fst $ head $ dropWhile (\(x,y) -> y < 500) $ reverse $ cumr x

solve2 :: Input -> Int
solve2 = foldr1 min . pam (distTriang <$> [0..2000])
