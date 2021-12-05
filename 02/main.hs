{-# LANGUAGE ViewPatterns #-}

import System.IO
import Control.Monad
import Data.List

main = do
  input <- parse <$> readFile "input"
  putStrLn (show $ solve1 input)
  putStrLn (show $ solve2 input)

parse :: String -> [(Int, Int)]
parse = map effect . lines
  where
    effect :: String -> (Int, Int)
    effect (stripPrefix "up "      -> Just val) = (- (read val) ,        0)
    effect (stripPrefix "down "    -> Just val) = (   read val  ,        0)
    effect (stripPrefix "forward " -> Just val) = (          0  , read val)
    effect _ = undefined

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

solve1 :: [(Int, Int)] -> Int
solve1 = uncurry (*) . both sum . unzip

solve2 :: [(Int, Int)] -> Int
solve2 = uncurry (*) . fst . foldl update ((0, 0), 0)
  where
    update :: ((Int, Int), Int) -> (Int, Int) -> ((Int, Int), Int)
    update ((v, h), t) (deltaT, deltaH) = ((v + deltaH * t, h + deltaH), t + deltaT)
