import Data.Array
import Data.List.Split
import Debug.Trace

type Memory = Array Int Int
type Position = Int

step :: (Memory, Position) -> (Memory, Position)
step (m, pc) = (m', pc + size) 
               where m' = case op of 1 -> m//[(operand 3, m ! (operand 1) + m ! (operand 2))]
                                     2 -> m//[(operand 3, m ! (operand 1) * m ! (operand 2))]
                     size = case op of 1 -> 4
                                       2 -> 4
                     operand nr = m ! (pc + nr)
                     op = m ! pc

run :: Memory -> Memory
run = fst . head . dropWhile notEnd . iterate step . initPc
       where initPc = \m -> (m, 0)
             currentOp = uncurry (!)
             notEnd = (/= 99) . currentOp

calc :: Memory -> Int -> Int -> Int
calc m noun verb = (run m') ! 0
                   where m' = m//[(1, noun), (2, verb)] 

day2 :: [Int] -> (Int, Int)
day2 input = head $ [(i, j) | i <- [0..99], j <- [0..99], calc memory i j == 19690720] 
     where memory = listArray (0, length input - 1) input

main :: IO ()
main = do input <- map read . splitOn "," <$> getContents
          -- print $ run (listArray (0, 4) [1,0,0,0,99])
          -- print $ run (listArray (0, 4) [2,3,0,3,99])
          -- print $ run (listArray (0, 5) [2,4,4,5,99,0])
          -- print $ run (listArray (0, 8) [1,1,1,4,99,5,6,0,99])
          print $ day2 input
