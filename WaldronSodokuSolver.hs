-- CS3210 - Principles of Programming Languages - Fall 2019
-- Programming Assignment 02 - A Sudoku Solver
-- Author(s): Christian Waldron
-- Date: 20 October 2019

import Data.List
import System.Environment
import System.IO
type Sequence = [Int]
type Board    = [Sequence]

split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = x : split d (drop 1 y) where (x,y) = span (/= d) s

toInt :: [Char] -> Int
toInt s = read s :: Int

toIntList :: [Char] -> Sequence
toIntList s = [ toInt [c] | c <- s ]
getNRows lst = length lst

getNColsHelper lst = length [d | d <- lst, length d == 9]

getNCols lst
   | getNColsHelper lst == 9 = 9
   | otherwise = 0

makeBoard lst = map toIntList lst

getBox lst x  y = [lst !! row !! col | row <- [3*x..3*x+2], col <- [3*y..3*y+2]]

getEmptySpot lst = [(row, col) | row <- [0..8], col <- [0..8], lst!!row!!col==0]!!0

isGridValid lst
   | getNRows lst + getNCols lst == 18 = True
   | otherwise = False

isSequenceValid lst
   | nub[d | d <- lst, d > 0] == [d | d <- lst, d > 0] = True
   | otherwise = False

areRowsValid lst
  |length[d | d <- lst, isSequenceValid d == True] == 9 = True
  | otherwise = False
  
areColsValid lst
  | areRowsValid (transpose lst) == True = True
  | otherwise = False


areBoxesValid lst 
   |and [isSequenceValid (getBox lst x y)|  x <- [0..2], y <- [0..2]] = True
   |otherwise = False

isValid lst
   |and [isGridValid lst, areRowsValid lst, areColsValid lst, areBoxesValid lst] = True
   |otherwise = False

isCompleted lst 
   |[elem 0 (lst!!x) | x <- [0..8]] == [False, False, False, False, False, False, False, False, False] = True
   |otherwise = False

isSolved lst
   |[isCompleted lst, isValid lst] == [True, True] = True
   |otherwise = False

setRowAt lst x y =
   if lst!!x == 0
   then concat [(take x lst), (concat [[y], (drop (x+1) lst)])] 
   else lst

setBoardAt lst x y z =
   if (lst!!x)!!y == 0
   then concat[(take x lst), (concat [[(setRowAt (lst!!x) y z)], (drop (x+1) lst)])]
   else lst

buildChoices lst x y = [setBoardAt lst x y z | z <- [1..9]]

solve board
   | isSolved board = [board]
   | isCompleted board = [[[]]]
   | not (isValid board) = [[[]]]
   | otherwise = concat [ solve choice | choice <- buildChoices board i j ]
     where
       emptySpot = getEmptySpot board
       i = fst emptySpot
       j = snd emptySpot

main = do
   args <- getArgs
   let fileName = head args
   contents <- readFile fileName
   let a = split '\n'contents
   let b = makeBoard a
   let c = [5,3,0,0,7,0,0,0,0]
   print [x| x <- solve b, x /= [[]]]
   print "Done!"
