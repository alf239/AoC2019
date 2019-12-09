import Control.Monad
import Data.List.Split
import Intcode

main :: IO ()
main = do program <- map read . splitOn "," <$> getContents

          putStrLn "=== Task 1 ==="
          print $ runInOut program [1]

          putStrLn "=== Task 2 ==="
          print $ runInOut program [5]
