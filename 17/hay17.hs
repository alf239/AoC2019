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
          let mm = Set.fromList [(x, y) | (l, y) <- zip (lines out) [0..], (c, x) <- zip l [0..], scaffold c]
          putStrLn out
          let iss = [(x, y) | (x, y) <- Set.toList mm
                            , Set.member (x+1, y) mm
                            , Set.member (x-1, y) mm
                            , Set.member (x, y+1) mm
                            , Set.member (x, y-1) mm
                            ]
          print iss
          print $ sum [x * y | (x, y) <- iss]

          putStrLn "=== Task 2 ==="
          let program = "A,A,B,C,B,C,B,C,C,A\nL,10,R,8,R,8\nL,10,L,12,R,8,R,10\nR,10,L,12,R,10\nn\n" -- Feel free to switch the "video feed" to 'y'
          let inp = map (fromIntegral . ord) program
          let results = runInOut (2 : tail input) inp
          putStrLn $ map (chr . fromIntegral) (init results)
          print $ last results
