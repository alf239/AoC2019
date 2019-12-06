import Control.Monad
import Data.Maybe
import Data.List
import Data.List.Split

type Wire = ((Int, Int), (Int, Int))

-- NOTE it does not catch all the intersections in collinear wires, may become a problem later
cross :: Wire -> Wire -> Maybe (Int, Int)
cross ((x1, y1), (x2, y2)) ((x3, y3), (x4, y4))
        | x1 == x2 && y3 == y4 && x3 <= x1 && x4 >= x1 && y1 <= y3 && y2 >= y3 = Just (x1, y3)
        | y1 == y2 && x3 == x4 && y3 <= y1 && y4 >= y1 && x1 <= x3 && x2 >= x3 = Just (x3, y1)
        | x1 == x2 && x3 == x4 && x1 == x3 && y3 >= y1 && y2 >= y3             = Just (x1, y3)
        | y1 == y2 && y3 == y4 && y1 == y3 && x3 >= x1 && x2 >= x3             = Just (x3, y2)
        | otherwise                                                            = Nothing

pairs :: [a] -> [(a, a)]
pairs = zip <*> tail

move :: (Int, Int) -> String -> (Int, Int)
move (x, y) ('U' : s) = (x, y + l) where l = read s
move (x, y) ('D' : s) = (x, y - l) where l = read s
move (x, y) ('L' : s) = (x - l, y) where l = read s
move (x, y) ('R' : s) = (x + l, y) where l = read s

normalize :: Wire -> Wire
normalize ((x1, y1), (x2, y2)) | x1 < x2 && y1 == y2  = ((x1, y1), (x2, y2)) 
normalize ((x1, y1), (x2, y2)) | x1 > x2 && y1 == y2  = ((x2, y2), (x1, y1)) 
normalize ((x1, y1), (x2, y2)) | x1 == x2 && y1 < y2  = ((x1, y1), (x2, y2)) 
normalize ((x1, y1), (x2, y2)) | x1 == x2 && y1 > y2  = ((x2, y2), (x1, y1)) 

wires :: String -> [Wire]
wires = pairs . scanl move (0, 0) . splitOn ","

crosses :: [Wire] -> [Wire] -> [(Int, Int)]
crosses as bs = catMaybes [cross p q | p <- map normalize as, q <- map normalize bs]

wire_len :: Wire -> Int
wire_len ((x1, y1), (x2, y2)) | x1 == x2  = abs (y2 - y1)
                              | otherwise = abs (x2 - x1) 

has_point :: Wire -> (Int, Int) -> Bool
has_point ((x1, y1), (x2, y2)) (x, y) 
              | x1 == x2 && x == x1   = y >= min y1 y2 && y <= max y1 y2
              | y1 == y2 && y == y1   = x >= min x1 x2 && x <= max x1 x2
              | otherwise             = False

wire_length :: [Wire] -> (Int, Int) -> Int
wire_length ws p = wire_length' 0 ws
   where wire_length' a (w' : rest) | w' `has_point` p = a + wire_len (fst w', p)
                                    | otherwise        = wire_length' (a + wire_len w') rest  

crossing :: String-> String -> String
crossing a b = let as = wires a
                   bs = wires b
                   xs = crosses as bs
                   valid = sort . map (\p -> (wire_length as p + wire_length bs p)) . filter (/= (0, 0)) $ xs
               in show valid


main :: IO ()
main = do [w1, w2] <- lines <$> getContents
          print $ crossing w1 w2
