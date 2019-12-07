import Control.Monad
import Data.List.Split
import Intcode

main :: IO ()
main = do input <- map read . splitOn "," <$> getContents
          let memory = toMemory input

          putStrLn "=== Task 1 ==="
          let (_, out1, _, _) = run ([1], memory)
          let result1 = unlines . map show . reverse $ out1
          putStrLn result1

          putStrLn "=== Task 2 ==="
          let (_, out2, _, _) = run ([5], memory)
          let result2 = unlines . map show . reverse $ out2
          putStrLn result2
