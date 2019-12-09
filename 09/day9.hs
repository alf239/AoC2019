import Control.Monad
import Data.List.Split
import Intcode

main :: IO ()
main = do input <- map read . splitOn "," <$> getContents

          putStrLn "=== Task 1 ==="
          let out = runInOut input [1]
          print out

          putStrLn "=== Task 2 ==="
          let out = runInOut input [2]
          print out

