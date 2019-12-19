module Main where

import Control.Monad
import Data.List.Split
import Data.Set (Set)
import qualified Data.Set as Set
import Intcode
import Data.Char

affected :: [Value] -> Value -> Value -> Bool
affected mm x y = runInOut mm [x, y] == [1]

main :: IO ()
main = do input <- map read . splitOn "," <$> getContents

          putStrLn "=== Task 1 ==="
          print . length $ [(x, y) | x <- [0..49], y <- [0..49], affected input x y]

          putStrLn . unlines $ [[if affected input x y then '#' else '.' | x <- [0..79]] | y <- [0..49]]

          putStrLn "=== Task 2 ==="
