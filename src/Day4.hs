module Day4
    ( run
      -- | test exports
    , evalLine
    , unzipDiagonalR0
    , unzipColumn0
    ) where

import Control.Monad (replicateM)
import Data.List.NonEmpty (NonEmpty, toList)

-------------------------------------------------------------------------------

run :: IO ()
run = run140

run140 :: IO ()
run140 = runForLines 140

run10 :: IO ()
run10 = runForLines 10

runForLines :: Int -> IO ()
runForLines n = do
    lines <- replicateM n getLine
    printEval lines

printEval :: [Line] -> IO ()
printEval = print . eval . Dataset

-------------------------------------------------------------------------------

type Line = [Char]
newtype Dataset = Dataset {unDataset :: [Line]}
newtype Diagonals = Diagonals {unDiagonals :: [Line]}

eval :: Dataset -> Int
eval ds@(Dataset ds') =
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
--
-- TODO: needed to test
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
