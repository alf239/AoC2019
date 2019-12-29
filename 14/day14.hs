module Main where

import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map

import Data.Int (Int64)
import Data.List (find)
import Data.List.Split (splitOn)

import Debug.Trace

type Amount = Int64

parse :: Read a => String -> (a, String, Map String a)
parse s = let [recipe, goal] = splitOn " => " s
              parse' s = let [n, name] = words s in (name, read n)
              (name, n) = parse' goal
              parts = map parse' . splitOn ", " $ recipe
          in (n, name, Map.fromList parts)

index :: [(a, String, Map String a)] -> Map String (a, Map String a)
index = Map.fromList . map (\(n, name, recipe) -> (name, (n, recipe)))

unknown :: Integral a => Map String a -> Maybe (String, a)
unknown = find (\(k, v) -> k /= "ORE" && v > 0). Map.toList

divCeil :: Integral a => a -> a -> a
divCeil a b = let (q, r) = a `divMod` b in if r > 0 then q + 1 else q

solve :: Integral a => Map String (a, Map String a) -> Map String a -> Map String a
solve env m = case unknown m of Nothing     -> m
                                Just (k, v) -> let (n, recipe) = env ! k
                                                   scale = v `divCeil` n
                                                   r' = Map.map (* scale) recipe 
                                                   q = scale * n
                                                   m' = Map.unionWith (+) m r'
                                                   m'' = Map.adjust (\a -> a - q) k m'
                                               in solve env m''

ore :: Integral a => Map String (a, Map String a) -> a -> a
ore env x = let solved = solve env (Map.singleton "FUEL" x)
            in solved ! "ORE"

binaryOpt :: Integral a => a -> a -> (a -> Bool) -> a
binaryOpt l h p | l >= (h - 1) = l
                | otherwise    = let m = (l + h) `div` 2
                                 in if p m then binaryOpt m h p
                                           else binaryOpt l m p

main :: IO ()
main = do rules <- map parse . lines <$> getContents
          let env = index rules
          putStrLn "=== Task 1 ==="
          print $ ore env 1

          let trillion = 1000000000000
          putStrLn "=== Task 2 ==="
          print $ binaryOpt 1 trillion ((<= trillion) . ore env)

