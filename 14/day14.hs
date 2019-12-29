module Main where

import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map

import Data.List (find)
import Data.List.Split (splitOn)

import Debug.Trace

parse :: String -> (Int, String, Map String Int)
parse s = let [recipe, goal] = splitOn " => " s
              parse' s = let [n, name] = words s in (name, read n)
              (name, n) = parse' goal
              parts = map parse' . splitOn ", " $ recipe
          in (n, name, Map.fromList parts)

index :: [(Int, String, Map String Int)] -> Map String (Int, Map String Int)
index = Map.fromList . map (\(n, name, recipe) -> (name, (n, recipe)))

unknown :: Map String Int -> Maybe (String, Int)
unknown = find (\(k, v) -> k /= "ORE" && v > 0). Map.toList

divCeil :: Int -> Int -> Int
divCeil a b = let (q, r) = a `divMod` b in if r > 0 then q + 1 else q

solve :: Map String (Int, Map String Int) -> Map String Int -> Map String Int
solve env m = case unknown m of Nothing     -> m
                                Just (k, v) -> let (n, recipe) = env ! (traceShowId k)
                                                   scale = v `divCeil` n
                                                   r' = Map.map (* scale) recipe 
                                                   q = scale * n
                                                   m' = Map.unionWith (+) m r'
                                                   m'' = Map.adjust (\a -> a - q) k m'
                                               in solve env (traceShowId m'')

main :: IO ()
main = do rules <- map parse . lines <$> getContents
          let env = index rules
          print $ solve env (Map.singleton "FUEL" 1)
