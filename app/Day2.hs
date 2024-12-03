module Day2 (runPart1, runPart2) where

import Control.Monad (replicateM)

{- FOURMOLU_DISABLE -}

-------------------------------------------------------------------------------
-- Part 1

runPart1 :: IO ()
runPart1 = parseInput >>= print . eval

parseInput :: IO [[Int]]
parseInput = replicateM 1000 (parseLine <$> getLine)

parseLine :: String -> [Int]
parseLine line = map read (words line)

eval :: [[Int]] -> Int
eval = foldl f 0
    where
        f acc l | isSafe  l = acc + 1
                | otherwise = acc

isSafe :: [Int] -> Bool
isSafe l = isSafeInc (isInc l) l

-- isIncremental
isInc :: [Int] -> Bool
isInc []            = True
isInc [_]           = True
isInc (x1 : x2 : _) = x1 < x2

isSafeInc :: Bool -> [Int] -> Bool
isSafeInc _ []  = True
isSafeInc _ [_] = True
isSafeInc isInc' (x1 : x2 : xs) =
    let diff = if isInc' then x2 - x1 else x1 - x2
     in (1 <= diff && diff <= 3) && isSafeInc isInc' (x2 : xs)

-------------------------------------------------------------------------------
-- Part 2

runPart2 :: IO ()
runPart2 = parseInput >>= print . eval2

eval2 :: [[Int]] -> Int
eval2 = foldl f 0
    where
        f acc l | isSafeToler l = acc + 1
                | otherwise     = acc

isSafeToler :: [Int] -> Bool
isSafeToler (x1 : x2 : x3 : xs)
    | x1 == x2             = isSafeTolerInc (isInc noX1) noX1 1
    | x1 < x2  && x2 > x3  = isSafeTolerInc False noX1 1 || isSafeTolerInc True  noX3 1 || isSafeTolerInc (isInc noX2) noX2 1
    | x1 > x2  && x2 < x3  = isSafeTolerInc True  noX1 1 || isSafeTolerInc False noX3 1 || isSafeTolerInc (isInc noX2) noX2 1
    where
        noX1 = x2 : x3 : xs
        noX2 = x1 : x3 : xs
        noX3 = x1 : x2 : xs
isSafeToler l = isSafeTolerInc (isInc l) l 0

type Failures = Int

isSafeTolerInc :: Bool -> [Int] -> Failures -> Bool
isSafeTolerInc isInc' = go []
    where
        go :: [Int] -> [Int] -> Failures -> Bool
        go _ _   2 = False
        go _ []  _ = True
        go _ [_] _ = True
        go [] (x1 : x2 : xs) fails
            | diff == 0 = go [] (x1 : xs) (fails + 1)
            | diff > 3  = go [] (x1 : xs) (fails + 1) || go [] (x2 : xs) (fails + 1)
            | diff < 0  = go [] (x1 : xs) (fails + 1) || go [] (x2 : xs) (fails + 1)
            | otherwise = go [x1] (x2 : xs) fails
            where
                diff = calDiff x1 x2
        go passed@(y : ys) (x1 : x2 : xs) fails
            | diff == 0 = go passed (x1 : xs) (fails + 1)
            | diff > 3  = go passed (x1 : xs) (fails + 1)
            | diff < 0  = go passed (x1 : xs) (fails + 1) || go ys (y : x2 : xs) (fails + 1)
            | otherwise = go (x1 : passed) (x2 : xs) fails
            where
                diff = calDiff x1 x2

        calDiff x1 x2 = if isInc' then x2 - x1 else x1 - x2
