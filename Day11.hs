-- Pair challengehacking with @mikemjharris (how this nightmare began for me)
-- This one seemed annoying but satisfying

module Day11 where
import Data.List (group, sort, nub, findIndex)
import Data.List.Split (chunksOf)

letters = ['a'..'h'] ++ ['j','k'] ++ ['m','n'] ++ ['p'..'z']

toLetter :: Int -> Char
toLetter i = letters !! i

fromLetter :: Char -> Int
fromLetter c = maybe (-1) id $ findIndex (==c) letters

increment :: String -> String
increment s = map toLetter $ increment' $ map fromLetter s
  where
    increment' :: [Int]->[Int]
    increment' l
      | last l == 22  = (increment' $ init l) ++ [0]
      | otherwise     = (init l) ++ [(last l) + 1]

allowed :: String -> Bool
allowed s = hasTwoPairs s && hasIncreasingStraight s

hasTwoPairs :: String -> Bool
hasTwoPairs s = (length pairs) >= 2
  where
    pairs = nub $ sort $ map snd $ filter (\(n,_)->(n>=2)) g
    g :: [(Int, Char)]
    g = map (\x-> (length x, head x)) $ group s

hasIncreasingStraight :: String -> Bool
hasIncreasingStraight s = any isIncreasingStraight chunks
  where
    isIncreasingStraight :: String -> Bool
    isIncreasingStraight [a,b,c] = (b == succ a) && (c == succ b)
    chunks :: [String]
    chunks = filter (\x -> (length x >= 3)) $ concatMap (chunksOf 3) $ [s, tail s, (tail $ tail s)]

answer input = take 2 $ filter allowed $ iterate increment input
