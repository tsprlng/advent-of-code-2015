module Day17 where
import Data.List (sort)

combos :: [Int]->[[Int]]
combos (c:cs) = let rest = combos cs in (map (c:) rest) ++ rest
combos [] = [[]]

main = do
  input <- getContents
  let containers = map read $ lines input :: [Int]
  let validCombos = map (\c->(length c, c)) $ filter ((==150).sum) $ combos containers
  putStrLn $ show.length $ validCombos

  let s = sort validCombos
  let efficientCombos = takeWhile ((==(fst $ head s)).fst) s
  putStrLn $ show.length $ efficientCombos
