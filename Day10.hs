-- Pair challengehacking with @mikemjharris (how this nightmare began for me)
-- This one actually seemed stupidly easy compared to the horrible time people were having in Py/Ruby

module Day10 where
import Data.List (group)

groupCounts :: [Int] -> [(Int, Int)]
groupCounts a = map (\x -> (length x, head x)) $ group a

step :: [Int] -> [Int]
step a = concatMap (\(n, x)-> [n,x]) $ groupCounts a

answer input = head $ drop 40 $ iterate step $ parse input

parse :: String -> [Int]
parse = map $ read.(:[])

example = length $ answer "1"
