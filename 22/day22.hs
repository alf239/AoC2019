module Main where

import Control.Monad
import Debug.Trace

dwi_ = "deal with increment "
cut_ = "cut "

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _          = True
isPrefixOf _ []          = False
isPrefixOf (a:as) (b:bs) | a == b    = as `isPrefixOf` bs
                         | otherwise = False 
 
new_stack :: Integer -> Integer -> Integer
new_stack n k = n - 1 - k

increment :: Integer -> Integer -> Integer -> Integer
increment n i k = (k * i) `mod` n

cut :: Integer -> Integer -> Integer -> Integer
cut n m k = (k - m + n) `mod` n

command :: Integer -> String -> Integer -> Integer
command n "deal into new stack"   = new_stack n
command n s | dwi_ `isPrefixOf` s = increment n (read . drop (length dwi_) $ s)
            | cut_ `isPrefixOf` s = cut n  (read . drop (length cut_) $ s)

main :: IO ()
main = do rules <- lines <$> getContents
          let n = 10007
          let start = 2019
          let result = foldl (\a s -> traceShow (a, s) $ command n s a) start rules
          print result


