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
type Circuit = Map.Map ValRef Input
type Cache = Map.Map ValRef Word16

data Input = Val ValRef | Fun FuncRef ValRef | BiFun FuncRef ValRef ValRef
  deriving Show

fix f = x where x = f x

calc :: Circuit -> Input -> Word16
calc circ v = fst $ calc' (Map.fromList []) v
  where
    calc' :: Cache -> Input -> (Word16, Cache)
    calc' cache (Val a) = maybe (get a) id $ readMaybe a
      where
        get :: ValRef -> (Word16, Cache)
        get a = maybe (set a) (\v -> (v,cache)) $ Map.lookup a cache
        set :: ValRef -> (Word16, Cache)
        set a = let (v, c') = (trace a $ calc' cache (circ Map.! a)) in (v, Map.insert a v c')
    calc' cache (Fun f a) = let (v,c') = calc' cache (Val a) in (apply f [v], c')
    calc' cache (BiFun f a a') = (apply f vals, c'')
      where
        vals = [v1, v2]
        (v1, c') = calc' cache (Val a) -- (how to map?)
        (v2, c'') = calc' c' (Val a') -- (how to map?)

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
