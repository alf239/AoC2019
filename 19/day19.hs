module Main where

import Control.Monad
import Control.Monad.Loops
import Data.List.Split
import Data.Set (Set)
import qualified Data.Set as Set
import Intcode
import Data.Char

affected :: [Value] -> Value -> Value -> Bool
affected mm x y = runInOut mm [x, y] == [1]




main :: IO ()
main = do input <- map read . splitOn "," <$> getContents
          let in_beam = affected input

          putStrLn "=== Task 1 ==="
          print . length $ [(x, y) | x <- [0..49], y <- [0..49], in_beam x y]

          putStrLn . unlines $ [[if in_beam x y then '#' else '.' | x <- [0..79]] | y <- [0..49]]

          putStrLn "=== Task 2 ==="
          

          let low = [(head . dropWhile (\x -> not $ in_beam x  y) $ [y..], y) | y <- [600..]]

          let suitable = [(x, y - 99) | (x, y) <- low, in_beam (x + 99) (y - 99)]

          let (xx, yy) = head suitable

          putStrLn . unlines $ [[if x >= xx && x < (xx + 100) && y >= yy && y < (yy + 100) then (if in_beam x y then 'O' else 'X')
                                                                                           else (if in_beam x y then '#' else '.') | x <- [(xx - 10)..(xx + 110)]] | y <- [(yy - 10)..(yy + 110)]]

          print $ xx * 10000 + yy


