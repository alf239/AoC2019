import Intcode

import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!), Map)
import Data.List.Split
import Debug.Trace

calc :: IntState -> Value -> Value -> Value
calc s noun verb = (! 0) . memory . run $ s'
                   where s' = s { memory = Map.insert 1 noun $ Map.insert 2 verb $ memory s }

day2 :: [Value] -> (Value, Value)
day2 m = head $ [(i, j) | i <- [0..99], j <- [0..99], calc start i j == 19690720]
     where start = fromInMemory [] m

main :: IO ()
main = do input <- map read . splitOn "," <$> getContents
          print $ day2 input
