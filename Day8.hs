module Day8 where
import Numeric (readHex)
import Data.Char (chr, isSpace)

unescape :: String -> String
unescape ('\\':'"' : s) = '"' : unescape s
unescape ('\\':'\\' : s) = '\\' : unescape s
unescape ('\\':'x':a:b : s) = let (c, rem) = (head $ readHex [a,b]) in chr c : rem ++ unescape s
unescape ('"':s) = unescape s
unescape (a:s) = let r = unescape s in if isSpace a then r else a:r
unescape s = s

escape :: String -> String
escape ('\\':s) = "\\\\" ++ escape s
escape ('"':s) = "\\\"" ++ escape s
escape (a:s) = a : escape s
escape s = s

main = do
  input <- getContents
  let i = filter (not.isSpace) input
  let l = length i
  let l' = length $ unescape i
  let l'' = sum $ map ((+2) . length . escape . filter (not.isSpace)) $ lines input

  mapM_ (putStrLn.show) [l,l',l'',0,l-l',l''-l]
