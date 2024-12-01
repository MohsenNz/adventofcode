module Challenge1 where

import Control.Monad (replicateM)
import Data.List (sort)
import GHC.Base (List)

run :: IO ()
run = parseInput >>= print . eval . sort'

parseInput :: IO (List (Int, Int))
parseInput = replicateM 1000 (parseLine <$> getLine)

parseLine :: List Char -> (Int, Int)
parseLine line =
    let l = map read (words line)
     in (l !! 0, l !! 1)

sort' :: List (Int, Int) -> List (Int, Int)
sort' l =
    let (ll, rl) = unzip l
     in zip (sort ll) (sort rl)

eval :: List (Int, Int) -> Int
eval =
    let f acc (x, y) = acc + abs (x - y)
     in foldl f 0
