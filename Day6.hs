module Day6 where

initialState n = replicate n (replicate n False)

data Fs = Never | Always | Flip
  deriving Show

f Never = \x->False
f Always = \x->True
f Flip = not

applyRange f (x1,x2) aa = safeX1 ++ map f inX ++ safeX2
  where
    (safeX1, inX) = splitAt (x1) hmmX
    (hmmX, safeX2) = splitAt (x2+1) aa

applySq f (x1,y1) (x2,y2) aa = applyRange (applyRange f (y1,y2)) (x1,x2) aa

applyLine :: String -> [[Bool]] -> [[Bool]]
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

followInstructions :: [String] -> [[Bool]] -> [[[Bool]]]
followInstructions instrs init = init : zipWith (\f x-> f x) (map applyLine instrs) (followInstructions instrs init)

conclude :: [String] -> [[Bool]]
conclude instrs = last $ followInstructions instrs (initialState 1000)

main = do
  input <- getContents
  putStrLn.show $ sum $ map (length . filter id) $ conclude $ lines input
