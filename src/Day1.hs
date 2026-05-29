module Day1 (run) where

import Control.Monad (replicateM)
import Data.List (sort)

run :: IO ()
run = parseInput >>= print . eval . sort'

parseInput :: IO [(Int, Int)]
parseInput = replicateM 1000 (parseLine <$> getLine)

parseLine :: String -> (Int, Int)
parseLine line =
    let (x1 : x2 : _) = map read (words line)
     in (x1, x2)

sort' :: [(Int, Int)] -> [(Int, Int)]
sort' l =
    let (ll, rl) = unzip l
     in zip (sort ll) (sort rl)

eval :: [(Int, Int)] -> Int
eval =
    let f acc (x, y) = acc + abs (x - y)
     in foldl f 0
