module Main where

import Control.Monad
import Data.List
import Debug.Trace

-- BORROWED: https://rosettacode.org/wiki/Modular_inverse#Haskell
--
-- Given a and m, return Just x such that ax = 1 mod m.
-- If there is no such x return Nothing.
modInv :: Integer -> Integer -> Maybe Integer
modInv a m
  | 1 == g    = Just (normalize m i)
  | otherwise = Nothing
  where
    (i, _, g) = gcdExt a m
 
-- Extended Euclidean algorithm.
-- Given non-negative a and b, return x, y and g
-- such that ax + by = g, where g = gcd(a,b).
-- Note that x or y may be negative.
gcdExt :: Integer -> Integer -> (Integer, Integer, Integer)
gcdExt a 0 = (1, 0, a)
gcdExt a b =
  let (q, r) = a `quotRem` b
      (s, t, g) = gcdExt b r
  in (t, s - q * t, g)
-- BORROWED - END

type Shuffle = (Integer, Integer)

normalize :: Integer -> Integer -> Integer
normalize n x | x < 0     = x + n
              | otherwise = x

modop :: Integer -> (Integer -> Integer -> Integer) -> Integer -> Integer -> Integer
modop n op a b = (a' `op` b') `mod` n
               where a' = normalize n a
                     b' = normalize n b

modadd :: Integer -> Integer -> Integer -> Integer
modadd n = modop n (+)

modmul :: Integer -> Integer -> Integer -> Integer
modmul n = modop n (*)

shzero :: Shuffle
shzero = (1, 0)

-- Applies (a, b) _after_ (p, q)
shcompose :: Integer -> Shuffle -> Shuffle -> Shuffle
shcompose n (a, b) (p, q) = (modmul n a p, modadd n b (modmul n a q))

power :: Integer -> Shuffle -> Integer -> Shuffle
power _ _ 0 = shzero
power _ a 1 = a
power n a b = shcompose n a' $! shcompose n p' p'
              where p' = power n a (b `div` 2)
                    a' = if odd b then a else shzero

new_stack :: Shuffle
new_stack = (-1, -1)

increment :: Integer -> Shuffle
increment i = (i, 0)

cut :: Integer -> Shuffle
cut m = (1, -m)

command :: String -> Shuffle
command "deal into new stack"   = new_stack
command s | dwi_ `isPrefixOf` s = increment (read . drop (length dwi_) $ s)
          | cut_ `isPrefixOf` s = cut (read . drop (length cut_) $ s)
    where dwi_ = "deal with increment "
          cut_ = "cut "

compose_all :: Integer -> [Shuffle] -> Shuffle
compose_all n = foldl' (\a c -> shcompose n c a) shzero

shuffle :: Integer -> Shuffle -> Integer -> Integer 
shuffle n (a, b) k = modadd n (modmul n a k) b

unshuffle :: Integer -> Shuffle -> Integer -> Integer 
unshuffle n (a, b) k = modmul n a' (modadd n k (negate b))
                       where Just a' = modInv a n

main :: IO ()
main = do rules <- lines <$> getContents
          let commands = map command rules

          putStrLn "=== Tests ==="
          print ("deal into new stack",       [shuffle 10 new_stack              x | x <- [0..9]])
          print ("deal into new stack twice", [shuffle 10 (power 10 new_stack 2) x | x <- [0..9]])
          print ("cut 3",                     [shuffle 10 (cut 3)                x | x <- [0..9]])
          print ("cut 3",                     [shuffle 10 (power 10 (cut 1) 3)   x | x <- [0..9]])
          print ("cut -4",                    [shuffle 10 (cut (negate 4))       x | x <- [0..9]])
          print ("deal with increment 3",     [shuffle 10 (increment 3)          x | x <- [0..9]])

          putStrLn "=== Task 1 ==="
          let n = 10007
          let result = compose_all n commands
          print $ shuffle n result 2019
          print $ unshuffle n result 5540

          putStrLn "=== Task 2 ==="
          let nn = 119315717514047
          let rr = 101741582076661
          let partial = compose_all nn commands
          let result2 = power nn partial rr
          let unshuffled = unshuffle nn result2 2020
          print unshuffled
          print $ shuffle nn result2 unshuffled

