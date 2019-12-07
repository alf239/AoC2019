import Intcode
import Data.Array
import Data.List.Split
import Debug.Trace
import Data.List
import Data.Maybe
import Control.Monad.State

thruster :: [Int] -> [Int] -> Int
thruster mm [pa, pb, pc, pd, pe] = let (_, [ra], _, _) = run ([pa, 0], m) 
                                       (_, [rb], _, _) = run ([pb, ra], m) 
                                       (_, [rc], _, _) = run ([pc, rb], m) 
                                       (_, [rd], _, _) = run ([pd, rc], m) 
                                       (_, [re], _, _) = run ([pe, rd], m) in re
                                  where m = toMemory mm

data ACS = Stopped
         | Ready (Int, Int -> ACS)

decide :: IntState -> ACS 
decide (is, [o], m, pc) = Ready (o, \i -> decide . runO $ (i : is, [], m, pc))
decide (_, [], _, _)    = Stopped

acs :: Memory -> Int -> Int -> ACS
acs m ph i = decide . runO $ ([ph, i], [], m, 0) 

asMaybe :: ACS -> Maybe (Int, Int -> ACS)
asMaybe Stopped = Nothing
asMaybe (Ready x) = Just x

step2 :: [Int -> ACS] -> Int -> Int
step2 [a, b, c, d, e] i = let out = do (ra, a') <- asMaybe $ a i
                                       (rb, b') <- asMaybe $ b ra
                                       (rc, c') <- asMaybe $ c rb
                                       (rd, d') <- asMaybe $ d rc
                                       (re, e') <- asMaybe $ e rd
                                       return $ step2 [a', b', c', d', e'] re
                           in fromMaybe i out


thruster2 :: [Int] -> [Int] -> Int
thruster2 mm [pa, pb, pc, pd, pe] = let Ready (ra, acsA) = acs m pa 0
                                        Ready (rb, acsB) = acs m pb ra
                                        Ready (rc, acsC) = acs m pc rb
                                        Ready (rd, acsD) = acs m pd rc
                                        Ready (re, acsE) = acs m pe rd in step2 [acsA, acsB, acsC, acsD, acsE] re
                                    where m = toMemory mm

outputs1 :: [Int] -> [Int]
outputs1 memory = [thruster memory phases | phases <- permutations [0, 1, 2, 3, 4]]


outputs2 :: [Int] -> [Int]
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
          let Ready (q, aa) = acs (toMemory [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]) 9 0
          print q
          let Ready (qq, aaa) = aa 2
          print qq
          print $ thruster2 [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5] [9, 8, 7, 6, 5]
          print $ thruster2 [3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10] [9, 7, 8, 5, 6]
          putStrLn "=== Task 2 result ==="
          let result2 = maximum . outputs2 $ input
          print result2

