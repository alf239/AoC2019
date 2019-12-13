module Main where

import Control.Monad
import qualified Data.Map.Strict as M
import Data.List.Split
import Intcode

main :: IO ()
main = do input <- map read . splitOn "," <$> getContents

          putStrLn "=== Task 1 ==="
          let out = runInOut input []
          let board = M.fromList $ [((x, y), z) | [x,y,z] <- chunksOf 3 out]
         
          print $ length . filter ((== 2) . snd) . M.toList $ board

