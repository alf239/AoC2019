import Intcode
import Data.List (permutations)
import Data.List.Split (splitOn)
import Control.Monad.State

acs :: [Value] ->  Value -> [Value] -> [Value]
acs m ph i = runInOut m (ph : i)

thruster :: [Value] -> [Value] -> Value
thruster mm [pa, pb, pc, pd, pe] = let a = acs mm pa
                                       b = acs mm pb
                                       c = acs mm pc
                                       d = acs mm pd
                                       e = acs mm pe
                                       full = e . d . c . b . a
                                    in last $ full [0]

thruster2 :: [Value] -> [Value] -> Value
thruster2 mm [pa, pb, pc, pd, pe] = let a = acs mm pa
                                        b = acs mm pb
                                        c = acs mm pc
                                        d = acs mm pd
                                        e = acs mm pe
                                        full = e . d . c . b . a 
                                        result = full (0 : result) 
                                     in last result

outputs1 :: [Value] -> [Value]
outputs1 memory = [thruster memory phases | phases <- permutations [0, 1, 2, 3, 4]]

outputs2 :: [Value] -> [Value]
outputs2 memory = [thruster2 memory phases | phases <- permutations [5, 6, 7, 8, 9]]

main :: IO ()
main = do input <- map read . splitOn "," <$> getContents
          
          putStrLn "=== Task 1 ==="
          print $ thruster [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0] [4, 3, 2, 1, 0]
          print $ thruster [3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0] [0, 1, 2, 3, 4]
          print $ thruster [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33, 1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0] [1, 0, 4, 3, 2]
          putStrLn "=== Task 1 result ==="
          let result1 = maximum . outputs1 $ input
          print result1

          putStrLn "=== Task 2 ==="
          print $ thruster2 [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5] [9, 8, 7, 6, 5]
          print $ thruster2 [3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10] [9, 7, 8, 5, 6]
          putStrLn "=== Task 2 result ==="
          let result2 = maximum . outputs2 $ input
          print result2

