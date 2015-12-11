module Day4 where
import Data.Digest.Pure.MD5 (md5)
import Data.ByteString.Lazy (pack)
import Data.Char (ord)

md5Str = show . md5 . pack . map (fromIntegral.ord) -- comically slow

startsWith a b = all id $ zipWith (==) a b

goodMatches p = filter (\x->(startsWith "00000" $ md5Str (p++x))) $ map show [1..]
goodMatches2 p = filter (\x->(startsWith "000000" $ md5Str (p++x))) $ map show [1..]
