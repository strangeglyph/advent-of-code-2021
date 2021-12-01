import System.IO
import Control.Monad
import Data.List

main = do
  input <- parse <$> readFile "input"
  putStrLn (show $ solve1 input)
  putStrLn (show $ solve2 input)

parse :: String -> [Int]
parse = map read . lines

solve1 :: [Int] -> Int
solve1 xs = length $ filter (< 0) $ zipWith (-) xs (tail xs)

solve2 :: [Int] -> Int
solve2 xs = solve1 windows
  where
    windows :: [Int]
    windows = map sum $ filter (\w -> length w == 3) $ transpose $ take 3 $ tails xs
