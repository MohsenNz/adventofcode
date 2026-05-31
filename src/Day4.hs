module Day4
    ( run_
    , runPart1
    , runPart2
      -- | test exports
    , evalLine
    , unzipDiagonalR0
    , unzipColumn0
    ) where

import Control.Monad (replicateM)
import Data.List.NonEmpty (NonEmpty, toList)

-------------------------------------------------------------------------------
-- Run

run_ :: IO ()
run_ = runPart2

runPart1 :: IO ()
runPart1 = run140 evalP1

runPart2 :: IO ()
runPart2 = run140 evalP2

run140 :: (Dataset -> Int) -> IO ()
run140 = runForLines 140

run10 :: (Dataset -> Int) -> IO ()
run10 = runForLines 10

runForLines :: Int -> (Dataset -> Int) -> IO ()
runForLines n eval = do
    lines <- replicateM n getLine
    printEval eval lines

printEval :: (Dataset -> Int) -> [Line] -> IO ()
printEval eval = print . eval . Dataset

-------------------------------------------------------------------------------
-- Part 1

type Line = [Char]
newtype Dataset = Dataset {unDataset :: [Line]}
newtype Diagonals = Diagonals {unDiagonals :: [Line]}

evalP1 :: Dataset -> Int
evalP1 ds@(Dataset ds') =
    evalH ds' + evalV ds' + evalDiagonalR ds + evalDiagonalL ds

evalH :: [Line] -> Int
evalH = foldr ((+) . evalLine) 0

evalV :: [Line] -> Int
evalV = go 0
  where
    go acc [] = acc
    go acc ds =
        let (col, ds') = unzipColumn0 ds
         in go (acc + evalLine col) ds'

unzipColumn0 :: [Line] -> (Line, [Line])
unzipColumn0 = go ([], [])
  where
    go :: (Line, [Line]) -> [Line] -> (Line, [Line])
    go acc [] = acc
    -- go (col, rest) [] = (col, reverse rest)
    go (col, ds') ((x : xs) : rest) = go (x : col, put xs ds') rest
    go acc ([] : ls) = error "unexpected"

evalDiagonalR :: Dataset -> Int
evalDiagonalR = evalH . unDiagonals . unzipDiagonalR

evalDiagonalL :: Dataset -> Int
evalDiagonalL = evalH . unDiagonals . unzipDiagonalL

type Pointer = Int
type DsWidth = Int
type DsHeight = Int

unzipDiagonalL :: Dataset -> Diagonals
unzipDiagonalL =
    unzipDiagonalR . Dataset . map reverse . unDataset

unzipDiagonalR :: Dataset -> Diagonals
unzipDiagonalR (Dataset ds) = Diagonals $ go [] ds 1
  where
    go :: [Line] -> [Line] -> Pointer -> [Line]
    go acc [] _ = acc
    go acc ds' pt =
        let (diagLine, ds'') = unzipDiagonalR0 width height ds' pt
         in go (diagLine : acc) ds'' (pt + 1)

    width = length $ head ds
    height = length ds

{- FOURMOLU_DISABLE -}
-- # Example
--  1 2 3                    2 3
--  4 5 6 , pt=0 -> ([1],    4 5 6)
--  7 8 9                    7 8 9
--
--- 2 3                      3
--  4 5 6 , pt=1 -> ([2, 4], 5 6  )
--  7 8 9                    7 8 9
--
-- # Note
--    * 3    * 3
--  * 5 6 =  * 5 6
--  7 8 9    7 8 9
unzipDiagonalR0
    :: DsWidth -> DsHeight -> [Line] -> Pointer -> (Line, [Line])
unzipDiagonalR0 width height ds pt
    | pt <= minWH               = go ([], []) ds pt
    | minWH < pt && pt <= maxWH = go ([], []) ds minWH
    | maxWH < pt                = go ([], []) ds (minWH + maxWH - pt)
    | width + height < pt       = error "pt is bigger than width and height"
  where
    go :: (Line, [Line]) -> [Line] -> Int -> (Line, [Line])
    go (diagonal, restLines) rest 0 = (diagonal, reverse restLines <> rest)
    go (diagonal, restLines) ((x : xs) : rest) i =
        go
            (x : diagonal, put xs restLines)
            rest
            (i - 1)

    minWH = min width height
    maxWH = max width height

put l ls = if null l then ls else l : ls

evalLine :: Line -> Int
evalLine = go 0
  where
    go acc ('X' : 'M' : 'A' : 'S' : xs) = go (acc + 1) ('S' : xs)
    go acc ('S' : 'A' : 'M' : 'X' : xs) = go (acc + 1) ('X' : xs)
    go acc (_ : xs)                     = go acc xs
    go acc []                           = acc

-------------------------------------------------------------------------------
-- Part 2

evalP2 :: Dataset -> Int
evalP2 (Dataset ds) = go 0 ds
  where
    go :: Int -> [Line] -> Int
    go acc [_, _] = acc
    go acc ls@(_ : ls') = go (acc + countXMasAtFirst3Lines ls) ls'

    countXMasAtFirst3Lines = f 0

    f :: Int -> [Line] -> Int
    f acc ([_, _]: _) = acc

    f acc ( (a1 : b1 : c1 : l1)
          : ( _ : b2 : c2 : l2)
          : (a3 : b3 : c3 : l3)
          : ls
          )                     | isXMas a1  c1
                                           b2
                                         a3  c3  = f (acc + 1) ls'
                                | otherwise      = f acc ls'
          where
            ls' = (b1 : c1 : l1)
                : (b2 : c2 : l2)
                : (b3 : c3 : l3)
                : ls

    isXMas 'M' 'S'
             'A'
           'M' 'S' = True

    isXMas 'M' 'M'
             'A'
           'S' 'S' = True

    isXMas 'S' 'S'
             'A'
           'M' 'M' = True

    isXMas 'S' 'M'
             'A'
           'S' 'M' = True

    isXMas  _   _
              _
            _   _  = False
