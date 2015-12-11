module Day1 where
import Data.List (inits)

val '(' = 1
val ')' = (-1)
val _   = 0

answer :: String -> Int
answer = sum . map val

answer2 :: String -> Int
answer2 = fst . head . filter (((-1)==).answer.snd) . zip [0..] . inits
