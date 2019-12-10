module Main where
import Data.Sort

data Point    = Point Int Int
                deriving (Show, Eq, Ord)
data Vector   = Vector Point Point
data Position = ON_SEGMENT | ON_LINE | LEFT | RIGHT
                deriving (Show, Eq, Ord)

area :: Vector -> Point -> Int
area (Vector (Point x1 y1) (Point x2 y2)) (Point x y) = (x2 - x1) * (y - y1) - (x - x1) * (y2 - y1)


within :: Vector -> Point -> Bool
within (Vector (Point x1 y1) (Point x2 y2)) (Point x y) = (x <= maxx) && (x >= minx) && (y <= maxy) && (y >= miny)
             where maxx = max x1 x2
                   minx = min x1 x2
                   maxy = max y1 y2
                   miny = min y1 y2

task :: Vector -> Point -> Position
task v p = if a == 0
           then (if within v p then ON_SEGMENT else ON_LINE)
           else (if a > 0 then LEFT else RIGHT)
           where a = area v p

detects :: [Point] -> Point -> Point -> Bool
detects sky a b | a == b    = False
                | otherwise = all (\p -> task (Vector a p) b /= ON_SEGMENT) (filter (/= b) . filter (/= a) $ sky) 



main :: IO ()
main = do input <- lines <$> getContents
          let sky = [Point x y | (y, l) <- zip [0..] input, (x, a) <- zip [0..] l, a == '#'] 
          let visibility = [(length $ filter (detects sky p) sky, p) | p <- sky]
          print $ take 10 . reverse . sort $ visibility
