module Day6 where

initialState n = replicate n (replicate n 0)

type Brightness = Int
data Fs = Never | Always | Flip
  deriving Show

f Never x
  | x <= 0 = 0
  | otherwise = x-1
f Always x = (x+1)
f Flip x = (x+2)

applyRange f (x1,x2) aa = safeX1 ++ map f inX ++ safeX2
  where
    (safeX1, inX) = splitAt (x1) hmmX
    (hmmX, safeX2) = splitAt (x2+1) aa

applySq f (x1,y1) (x2,y2) aa = applyRange (applyRange f (y1,y2)) (x1,x2) aa

applyLine :: String -> [[Brightness]] -> [[Brightness]]
applyLine l = applySq (f fs) (x1,y1) (x2,y2)
  where
    (fs, (x1,y1):(x2,y2):[]) = interpretLine l

interpretLine :: String -> (Fs, [(Int, Int)])
interpretLine l = i $ words l
  where
    i ("turn":"on":rest)  = (Always, c rest)
    i ("turn":"off":rest) = (Never, c rest)
    i ("toggle":rest)     = (Flip, c rest)
    c (a:"through":b:_)   = map (\(a,b)->(read a, (read.tail) b)) $ map (break (==',')) [a,b]

followInstructions :: [String] -> [[Brightness]] -> [[[Brightness]]]
followInstructions instrs init = init : zipWith (\f x-> f x) (map applyLine instrs) (followInstructions instrs init)

conclude :: [String] -> [[Brightness]]
conclude instrs = last $ followInstructions instrs (initialState 1000)

main = do
  input <- getContents
  putStrLn.show $ sum $ map sum $ conclude $ lines input
