module Day2 where
import Data.List (sort)
import Data.Text (splitOn, pack, unpack)

onePres w h d = 2 * sum areas + head areas
  where
    areas = sort [(w * h), (w * d), (h * d)]

onePres2 w h d = (2 * sum (take 2 $ sort [w,h,d])) + (w * h * d)

oneLine f = (\[a,b,c]->f a b c) . map (read.unpack) . (splitOn $ pack "x") . pack

main = do
  input <- getContents
  putStrLn.show $ sum $ map (oneLine onePres2) $ lines input
