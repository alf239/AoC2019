import Data.Array
import Data.List.Split
import Debug.Trace

type Memory = Array Int Int
type Address = Int
type In = [Int]
type Out = [Int]

step :: (In, Out, Memory, Address) -> (In, Out, Memory, Address)
step (i, o, m, pc) = (i', o', m', pc') 
               where i' = case op of 3 -> tail i
                                     _ -> i
                     o' = case op of 4 -> operand 1 : o
                                     _ -> o
                     m' = case op of 1 -> m//[(operand' 3, operand 1 + operand 2)]
                                     2 -> m//[(operand' 3, operand 1 * operand 2)]
                                     3 -> m//[(operand' 1, head i)]
                                     7 -> m//[(operand' 3, fromEnum $ operand 1 < operand 2)]
                                     8 -> m//[(operand' 3, fromEnum $ operand 1 == operand 2)]
                                     _ -> m
                     pc' = case op of 5 | operand 1 /= 0 -> operand 2
                                      6 | operand 1 == 0 -> operand 2
                                      _                  -> pc + size
                     size = [-1, 4, 4, 2, 2, 3, 3, 4, 4] !! op
                     parmode n = (opcode `div` (10 ^ (n + 1))) `mod` 10
                     operand' nr = m ! (pc + nr)
                     operand nr = let value = operand' nr
                                   in if parmode nr == 1 then value else m ! value
                     op = opcode `mod` 100
                     opcode = m ! pc

run :: (In, Memory) -> (In, Out, Memory, Address)
run = head . dropWhile notEnd . iterate step . initComputer
       where initComputer = \im -> case im of (i, m) -> (i, [], m, 0)
             currentOp = \state -> case state of (_, _, m, pc) -> m ! pc
             notEnd = (/= 99) . currentOp

main :: IO ()
main = do input <- map read . splitOn "," <$> getContents
          let memory = listArray (0, length input - 1) input
          let (_, out, _, _) = run ([5], memory)
          let result = unlines . map show . reverse $ out
          putStrLn result
