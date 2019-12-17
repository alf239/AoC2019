module Main where

import Control.Monad
import Data.List.Split
import Data.Set (Set)
import qualified Data.Set as Set
import Intcode
import Data.Char

scaffold :: Char -> Bool
scaffold '#' = True
scaffold '^' = True
scaffold '>' = True
scaffold '<' = True
scaffold 'v' = True
scaffold _   = False


main :: IO ()
main = do input <- map read . splitOn "," <$> getContents

          putStrLn "=== Task 1 ==="
          let out = map (chr . fromIntegral) . runInOut input  $ []
          let map = Set.fromList [(x, y) | (l, y) <- zip (lines out) [0..], (c, x) <- zip l [0..], scaffold c]
          putStrLn out
          let iss = [(x, y) | (x, y) <- Set.toList map
                            , Set.member (x+1, y) map
                            , Set.member (x-1, y) map
                            , Set.member (x, y+1) map
                            , Set.member (x, y-1) map
                            ]
          print iss
          print $ sum [x * y | (x, y) <- iss]

