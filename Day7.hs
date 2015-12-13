module Day7 where
import qualified Data.Map as Map
import Data.Bits  -- try to qualify these while keeping the instance Bits Int16
import Data.Word
import Text.Read ( readMaybe )
import Debug.Trace
import Data.Function.Memoize

type FuncRef = String
type ValRef = String
type Val = Word16

data Input = Val ValRef | Fun FuncRef ValRef | BiFun FuncRef ValRef ValRef
  deriving Show

fix f = x where x = f x

calc :: Map.Map ValRef Input -> Input -> Word16
calc circ v = fix calc' $ v
  where
    calc' rec (Val a) = maybe (trace a $ rec (circ Map.! a)) id $ readMaybe a
    calc' rec (Fun f a) = apply f $ [rec (Val a)]
    calc' rec (BiFun f a a') = apply f $ map (\x-> rec (Val x)) [a,a'] -- ugh this should be a tuple (how to map?)

apply :: String -> [Word16] -> Word16
apply "OR" [a,a'] = a .|. a'
apply "AND" [a,a'] = a .&. a'
apply "LSHIFT" [a,s] = shiftL a $ fromIntegral s
apply "RSHIFT" [a,s] = shiftR a $ fromIntegral s
apply "NOT" [a] = complement a
apply wtf _ = error $ "Wtf is " ++ wtf

interpretLine l' = i l
  where
    l = words l'
    i [v,"->",k] = (k, Val v)
    i [f,v,"->",k] = (k, Fun f v)
    i [v,f,v',"->",k] = (k, BiFun f v v')

main = do
  circuit <- fmap (Map.fromList . map interpretLine . lines) getContents  -- h8 this
  putStrLn.show $ calc circuit (Val "a")
