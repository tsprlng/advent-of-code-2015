module Day5 where
import Data.List (group, isInfixOf)

isNiiice s = threeVowels s && aPair s && noCrap s

threeVowels s = (>=3) $ length $ filter (flip elem "aoeui") s
aPair s = any ((>=2).length) $ group s
noCrap s = all (not . (`isInfixOf` s)) ["ab","cd","pq","xy"]

isNiiice2 s = hasRepPair s && hasSandwich s

hasRepPair s = any (\(a:b:rest)-> [a,b] `isInfixOf` rest) $ suffixes s
hasSandwich s = any (\(a:b:c:rest)-> a==c) $ suffixes s
suffixes s = takeWhile ((>=3).length) $ iterate (drop 1) s

main = do
  input <- getContents
  putStrLn.show $ length $ filter isNiiice2 $ lines input
