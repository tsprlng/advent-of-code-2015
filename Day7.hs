module Day7 where
import qualified Data.Map as Map

interpretLine l' = i l
  where
    l = words l'
    i [v,'->',k] = (k, Val v)
    i [f,v,'->',k] = (k, Fun f v)
    i [v,f,v','->',k] = (k, BiFun f v v')
main = do
  circuit <- fmap (Map.fromList . map interpretLine . lines) getContents  -- h8 this
  putStrLn.show $ circuit
