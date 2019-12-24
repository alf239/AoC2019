module Main where

import Data.Bits 
import Data.Int (Int64)
import Data.List (find, foldl')
import Data.Set (Set)
import qualified Data.Set as Set

type Coords = (Int, Int, Int)
type Scan = Set (Coords)

input = [ "#####"
        , "....."
        , "....#"
        , "#####"
        , ".###."
        ]

parse :: [String] -> Scan
parse ss = Set.fromList [(x, y, 0) | (l, y) <- zip ss [0..], (c, x) <- zip l [0..], c == '#']

biodiversity :: Scan -> Int64
biodiversity ss = sum [bit (5 * y + x) | x <- [0..4], y <- [0..4], bug ss (x, y, 0)]

bug :: Scan -> (Coords) -> Bool
bug ss a = Set.member a ss

neigbours :: (Coords) -> [(Coords)]
neigbours (x, y, 0) = [(x - 1, y, 0), (x + 1, y, 0), (x, y - 1, 0), (x, y + 1, 0)]

population :: Scan -> Coords -> Int
population ss = length . filter (bug ss) . neigbours

rule :: Scan -> Coords -> Bool
rule ss p = case (bug ss p, population ss p) of
                    (True, 1) -> True
                    (False, 1) -> True
                    (False, 2) -> True
                    otherwise -> False

step :: Scan -> Scan
step ss = Set.fromList . filter (rule ss) $ [(x, y, 0) | x <- [0..4], y <- [0..4]]

draw :: Scan -> String
draw ss = unlines [[if bug ss (x, y, 0) then '#' else '.' | x <- [0..4]] | y <- [0..4]]

main :: IO ()
main = do let task = parse input

          let hashed = iterate (\(p, s) -> (step p, Set.insert p s)) (task, Set.empty)

          let Just result = find (\(p, s) -> Set.member p s) hashed
          print . biodiversity . fst $ result

