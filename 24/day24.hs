module Main where

import Data.Bits 
import Data.Int (Int64)
import Data.List (find, foldl')
import Data.Set (Set)
import qualified Data.Set as Set

type Coords = (Int, Int, Int)
data Scan = Scan {
              bugs :: Set (Coords),
              zmax :: Int,
              zmin :: Int 
            } deriving (Eq, Ord, Show)

input = [ "#####"
        , "....."
        , "....#"
        , "#####"
        , ".###."
        ]

sample = [ "....#"
         , "#..#."
         , "#..##"
         , "..#.."
         , "#...."
         ]

fromBugs :: [Coords] -> Scan
fromBugs bs = Scan { bugs = Set.fromList bs, zmax = maximum zs, zmin = minimum zs }
              where zs = [z | (_, _, z) <- bs]

parse :: [String] -> Scan
parse ss = fromBugs [(x, y, 0) | (l, y) <- zip ss [0..], (c, x) <- zip l [0..], c == '#']

biodiversity :: Scan -> Int64
biodiversity ss = sum [bit (5 * y + x) | x <- [0..4], y <- [0..4], bug ss (x, y, 0)]

bug :: Scan -> (Coords) -> Bool
bug ss a = Set.member a (bugs ss)

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
step ss = fromBugs . filter (rule ss) $ [(x, y, 0) | x <- [0..4], y <- [0..4]]

neigbours2 :: (Coords) -> [(Coords)]
neigbours2 (0, 0, z) = [(1, 0, z), (0, 1, z), (1, 2, z - 1), (2, 1, z - 1)]
neigbours2 (4, 0, z) = [(3, 0, z), (4, 1, z), (2, 1, z - 1), (3, 2, z - 1)]
neigbours2 (0, 4, z) = [(1, 4, z), (0, 3, z), (1, 2, z - 1), (2, 3, z - 1)]
neigbours2 (4, 4, z) = [(3, 4, z), (4, 3, z), (3, 2, z - 1), (2, 3, z - 1)]
neigbours2 (0, y, z) = [(1, y, z), (0, y - 1, z), (0, y + 1, z), (1, 2, z - 1)]
neigbours2 (4, y, z) = [(3, y, z), (4, y - 1, z), (4, y + 1, z), (3, 2, z - 1)]
neigbours2 (x, 0, z) = [(x, 1, z), (x - 1, 0, z), (x + 1, 0, z), (2, 1, z - 1)]
neigbours2 (x, 4, z) = [(x, 3, z), (x - 1, 4, z), (x + 1, 4, z), (2, 3, z - 1)]
neigbours2 (1, 2, z) = [(0, 2, z), (1, 1, z), (1, 3, z)] ++ [(0, y, z + 1) | y <- [0..4]]
neigbours2 (2, 1, z) = [(2, 0, z), (1, 1, z), (3, 1, z)] ++ [(x, 0, z + 1) | x <- [0..4]]
neigbours2 (3, 2, z) = [(4, 2, z), (3, 1, z), (3, 3, z)] ++ [(4, y, z + 1) | y <- [0..4]]
neigbours2 (2, 3, z) = [(2, 4, z), (1, 3, z), (3, 3, z)] ++ [(x, 4, z + 1) | x <- [0..4]]
neigbours2 (x, y, z) = [(x - 1, y, z), (x + 1, y, z), (x, y - 1, z), (x, y + 1, z)]

population2 :: Scan -> Coords -> Int
population2 ss = length . filter (bug ss) . neigbours2

rule2 :: Scan -> Coords -> Bool
rule2 ss p = case (bug ss p, population2 ss p) of
                    (True, 1) -> True
                    (False, 1) -> True
                    (False, 2) -> True
                    otherwise -> False

step2 :: Scan -> Scan
step2 ss = fromBugs . filter (rule2 ss) $ [(x, y, z) | x <- [0..4], y <- [0..4], (x, y) /= (2, 2), z <- [zmin ss - 1..zmax ss + 1]]

draw :: Scan -> String
draw ss = unlines [[if bug ss (x, y, 0) then '#' else '.' | x <- [0..4]] | y <- [0..4]]

draw2 :: Scan -> String
draw2 ss = unlines . concat $ [["Depth " ++ show z ++ ":"] ++ 
                               [[if bug ss (x, y, z) then '#' else '.' | x <- [0..4]] | y <- [0..4]] ++ 
                               [""]
                               | z <- [zmin ss..zmax ss]]

main :: IO ()
main = do let task = parse input
          let smpl = parse sample

          let hashed = iterate (\(p, s) -> (step p, Set.insert p s)) (task, Set.empty)

          let Just result = find (\(p, s) -> Set.member p s) hashed
          print . biodiversity . fst $ result

          let recursive = iterate step2 task

          -- putStrLn . draw2 $ iterate step2 smpl !! 10

          print . Set.size . bugs $ recursive !! 200
