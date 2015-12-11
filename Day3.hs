module Day3 where
import Data.List (nub, sort, transpose)
import Data.List.Split (chunksOf)

move :: Char -> (Int,Int) -> (Int,Int)
move '>' (a,b) = (a+1, b)
move '<' (a,b) = (a-1, b)
move '^' (a,b) = (a, b+1)
move 'v' (a,b) = (a, b-1)

visits :: (Int,Int) -> [Char] -> [(Int, Int)]
visits init instrs = init : zipWith move instrs (visits init instrs)

visited instrs = length.nub.sort $ visits (0,0) instrs

visited2 instrs = length.nub.sort $ concatMap (visits (0,0)) $ transpose $ chunksOf 2 instrs
