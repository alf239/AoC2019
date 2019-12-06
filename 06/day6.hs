import qualified Data.Map.Strict as Map
import Data.Map.Strict((!))
import Data.List 
import Data.List.Split
import Debug.Trace

tuplify :: [String] -> (String, String)
tuplify [x, y] = (x, y)

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

costs :: Map.Map String [String] -> String -> Map.Map String Integer -> Integer -> Map.Map String Integer
costs m s acc l = case Map.lookup s m of Just vs -> foldl' (\acc' s' -> costs m s' acc' (l+1)) acc'' vs
                                         Nothing -> acc''
                  where acc'' = Map.insert s l acc

distance :: Map.Map String String -> Map.Map String Integer -> Integer -> String -> String -> Integer
distance m cs a y s = let yp = m ! y
                          sp = m ! s
                          yc = cs ! y
                          sc = cs ! s
                       in if yp == sp then a
                          else if yc == sc then distance m cs (a + 2) yp sp
                          else if yc < sc then distance m cs (a + 1) y sp
                          else distance m cs (a + 1) yp s

main :: IO ()
main = do input <- map (splitOn ")") . lines <$> getContents
          let orbits = Map.fromList . map (swap . tuplify) $ input
          let planets = concat input
          let Just(start) = find (\x -> Map.notMember x orbits) planets
          let center = Map.singleton start []
          let tree = foldl' (\m [c, o] -> Map.alter (\os -> case os of Nothing -> Just [o]
                                                                       Just os'-> Just $ o : os') c m) center input
          let cs = costs tree start (Map.singleton start 0) 0
          let cost = Map.foldr (+) 0 cs
          print tree
          print ("task1", cost) -- changed from 42 to 54 due to YOU and SAN
          print ("task2", distance orbits cs 0 "YOU" "SAN")
