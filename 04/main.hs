{-# LANGUAGE ViewPatterns, DuplicateRecordFields #-}

import System.IO
import Control.Monad
import Data.List
import Data.Maybe
import Debug.Trace

data Board = Board { fields :: [[(Int, Bool)]] } deriving (Show, Eq)
data Bingo = Bingo { inputs :: [Int], boards :: [Board], lastInput :: Int } deriving (Show, Eq)

main = do
  input <- parse <$> readFile "input"
  putStrLn (show $ solve1 input)
  putStrLn (show $ solve2 input)

parse :: String -> Bingo
parse txt = let ls = lines txt
                inputs = parseInputs (head ls)
                boards = parseBoards (tail ls) in
            Bingo { inputs = inputs, boards = boards, lastInput = -1 }

parseInputs :: String -> [Int]
parseInputs = map read . split ','

parseBoards :: [String] -> [Board]
parseBoards (blank:a:b:c:d:e:rest) = Board { fields =
                                               [ gameLine a,
                                                 gameLine b,
                                                 gameLine c,
                                                 gameLine d,
                                                 gameLine e]
                                           } : parseBoards rest
  where
    gameLine :: String -> [(Int, Bool)]
    gameLine = map (injR False) . map read . filter (/= []) . split ' '
parseBoards _ = []

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


isBoardWinning :: Board -> Bool
isBoardWinning (Board { fields = fields }) =
  let markings = mapp snd fields in
      or (map and markings)
      || or (map and $ transpose markings)

unmarkedScore :: Board -> Int
unmarkedScore = sum . map sum . mapp fst . map (filter (\(_, m) -> not m)) . fields

mark :: Int -> Board -> Board
mark val (Board { fields = f }) = Board { fields = mapp (subst (val, False) (val, True)) f }


applyStep :: Bingo -> Maybe Bingo
applyStep (Bingo { inputs = (x:xs), boards = boards }) = Just Bingo { inputs = xs, boards = map (mark x) boards, lastInput = x }
applyStep _ = Nothing

solve1 :: Bingo -> Int
solve1 bingo = let winningStep = fromJust $ find (any isBoardWinning . boards) seq
                   winningNumber = lastInput winningStep
                   winningBoard = fromJust $ find isBoardWinning $ boards winningStep
               in  unmarkedScore (winningBoard) * winningNumber
  where
    seq :: [Bingo]
    seq = unfoldr (\b -> fork id applyStep <$> b) (Just bingo)

solve2 :: Bingo -> Int
solve2 bingo = let winningStep = head $ dropWhile (any (not . isBoardWinning) . boards) seq
                   prevStep = head $ reverse $ takeWhile (any (not . isBoardWinning) . boards) seq
                   winningNumber = lastInput winningStep
                   winningBoard = fst $ fromJust $ findMismatch isBoardWinning (boards winningStep) (boards prevStep)
               in  unmarkedScore (winningBoard) * winningNumber
  where
    seq :: [Bingo]
    seq = unfoldr (\b -> fork id applyStep <$> b) (Just bingo)
    findMismatch :: (a -> Bool) -> [a] -> [a] -> Maybe (a, a)
    findMismatch f _ [] = Nothing
    findMismatch f [] _ = Nothing
    findMismatch f (x:xs) (y:ys)
      | f x && not (f y)   = Just (x, y)
      | not (f x) && f y   = Just (x, y)
      | otherwise          = findMismatch f xs ys
