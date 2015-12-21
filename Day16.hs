module Day16 where
import Data.Map (Map, fromList, assocs, lookup)
import Data.Text (splitOn, pack, unpack, strip)  -- TODO argh no not this crap again

type KnowledgeOfSue = Map String Int
type SueConstraints = Map String (Int->Bool)

interpretLine :: String -> KnowledgeOfSue
interpretLine (':':s) = interpretActualLine s  -- Throw away the first bit with sue number
  where
    interpretActualLine s = fromList $ Prelude.map (interpretPair . unpack) $ ((splitOn $ pack ",") . pack) s
    interpretPair s = (a, read b) where [a,b] = Prelude.map (unpack . strip) $ ((splitOn $ pack ":") . pack) s
interpretLine (a:s) = interpretLine s

knowledgeOfSue :: SueConstraints
knowledgeOfSue = fromList
  [
    ("children", (==3)),
    ("cats", (>7)),
    ("samoyeds", (==2)),
    ("pomeranians", (<3)),
    ("akitas", (==0)),
    ("vizslas", (==0)),
    ("goldfish", (<5)),
    ("trees", (>3)),
    ("cars", (==2)),
    ("perfumes", (==1))
  ]

matches :: SueConstraints -> KnowledgeOfSue -> Bool
matches constraints thisSue = all match $ assocs thisSue
  where
    match (thing, number) = maybe True (\x->x number) $ Data.Map.lookup thing constraints

main = do
  input <- getContents
  let sues = map snd $ filter (matches knowledgeOfSue . fst) $ zip (map interpretLine $ lines input) [1..]
  putStrLn $ show sues
