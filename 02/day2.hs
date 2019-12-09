import Intcode

import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!), Map)
import Data.List.Split
import Debug.Trace

calc :: [Value] -> Value -> Value -> Value
calc m noun verb = head . runMem $ mem
                   where mem = head m : noun : verb : drop 3 m

day2 :: [Value] -> (Value, Value)
day2 m = head $ [(i, j) | i <- [0..99], j <- [0..99], calc m i j == 19690720]

main :: IO ()
main = do input <- map read . splitOn "," <$> getContents
          print $ day2 input
