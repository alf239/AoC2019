module Main where

import Data.Int (Int64)
import Data.Char (digitToInt)
import Data.List (scanl')

input = "59738571488265718089358904960114455280973585922664604231570733151978336391124265667937788506879073944958411270241510791284757734034790319100185375919394328222644897570527214451044757312242600574353568346245764353769293536616467729923693209336874623429206418395498129094105619169880166958902855461622600841062466017030859476352821921910265996487329020467621714808665711053916709619048510429655689461607438170767108694118419011350540476627272614676919542728299869247813713586665464823624393342098676116916475052995741277706794475619032833146441996338192744444491539626122725710939892200153464936225009531836069741189390642278774113797883240104687033645"
sample = "12345678"

base_pattern = [0, 1, 0, -1]

pattern :: Int -> [Int64]
pattern n = tail . cycle . concatMap (take n . repeat) $ base_pattern

digit :: Int -> [Int64] -> Int64
digit n = (`mod` 10) . abs . sum . zipWith (*) (pattern n)

parse :: String -> [Int64]
parse = map (fromIntegral . digitToInt)      

phase :: [Int64] -> [Int64]
phase xs = [digit n $! xs | n <- [1..length xs]]

addm :: Int64 -> Int64 -> Int64
addm x y = (x + y) `mod` 10

print_prefix :: Show a => [a] -> IO ()
print_prefix = putStrLn . concat . map show . take 8

main :: IO ()
main = do let src = parse input
          print_prefix $ iterate phase src !! 100

          let offset = read . take 7 $ input
          let real_src = concat . take 10000 . repeat $ src
          let src' = reverse . drop offset $ real_src 
          
          print_prefix . reverse $ iterate (scanl' addm 0) src' !! 100


