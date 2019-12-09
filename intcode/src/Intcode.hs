module Intcode where

import qualified Data.Map.Strict as Map
import Data.Int
import Data.Map.Strict ((!), Map)
import Data.List.Split
import Debug.Trace
import Data.List
import Data.Maybe
import Control.Monad.State

type Value = Int64
type Memory = Map Value Value
type Address = Value
type In = [Value]
type Out = [Value]
data IntState = IntState { input :: In
                         , output :: Out
                         , memory :: Memory
                         , pc :: Address
                         , base :: Address
                         } deriving (Show)   


currentOp :: IntState -> Value
currentOp s = (memory s) ! (pc s)

halted :: IntState -> Bool
halted = (== 99) . currentOp

opcode :: State IntState Int64
opcode = gets currentOp

fetch :: Int64 -> State IntState Int64
fetch addr = do m <- gets memory
                return $ if addr < 0 then error("Negative address")
                                     else Map.findWithDefault 0 addr m

jmp :: (Int64 -> Int64) -> State IntState ()
jmp f = do s <- get
           put $ s { pc = f (pc s)}

store :: Address -> Int64 -> State IntState ()
store a v = modify $ \s -> s { memory = Map.insert a v (memory s) }

addr :: Int64 -> State IntState Int64
addr n = do op <- opcode
            b <- gets base
            pc' <- gets pc
            let mode = op `div` 10 ^ (n + 1) `mod` 10
            case mode of 2 -> (+ b) <$> fetch (pc' + n)
                         1 -> return (pc' + n)
                         0 -> fetch (pc' + n)

withBase :: Int64 -> State IntState ()
withBase b = modify $ \s -> s { base = b }

setBase :: State IntState ()
setBase = do b <- gets base
             a <- addr 1 >>= fetch
             withBase $ a + b
             jmp (+ 2)

readIo :: State IntState Int64
readIo = do i <- gets input
            _ <- modify $ \s -> s { input = tail i }
            return $ head i

writeIo :: Int64 -> State IntState ()
writeIo x = modify $ \s -> s { output = x : (output s) }

binaryOp :: (Int64 -> Int64 -> Int64) -> State IntState ()
binaryOp op = do a <- addr 1 >>= fetch
                 b <- addr 2 >>= fetch
                 c <- addr 3
                 let v = a `op` b
                 _ <- store c v
                 jmp (+ 4)

jif :: (Int64 -> Bool) -> State IntState ()
jif p = do a <- addr 1 >>= fetch
           b <- addr 2 >>= fetch
           if p a then jmp (const b)
                  else jmp (+ 3) 

readInput :: State IntState ()
readInput = do a <- addr 1
               v <- readIo
               _ <- store a v
               jmp (+ 2)

writeOutput :: State IntState ()
writeOutput = do o <- gets output
                 a <- addr 1 >>= fetch
                 _ <- writeIo a
                 jmp (+ 2)

stepS :: State IntState ()
stepS = do op <- (`mod` 100) <$> opcode
           case op
             of 1 -> binaryOp (+)
                2 -> binaryOp (*)
                3 -> readInput
                4 -> writeOutput
                5 -> jif (/= 0)
                6 -> jif (== 0)
                7 -> binaryOp (\a b -> if a < b then 1 else 0)
                8 -> binaryOp (\a b -> if a == b then 1 else 0)
                9 -> setBase 
                99 -> return ()

step :: IntState -> IntState 
step = execState stepS

run :: (In, Memory) -> IntState 
run = head . dropWhile notEnd . iterate step . initComputer
       where initComputer = \im -> case im of (i, m) -> IntState { input = i
                                                                 , output = []
                                                                 , memory = m
                                                                 , pc = 0
                                                                 , base = 0
                                                                 }
             notEnd = not . halted

runO :: IntState -> IntState
runO = head . dropWhile nonBlocking . iterate step
       where nonBlocking s = notEnd s && (null . output $ s)
             notEnd = not . halted

toMemory :: [Int64] -> Memory
toMemory = Map.fromList . zip [0..]

runMem :: [Int64] -> [Int64]
runMem = Map.elems . memory . run . (\m -> ([], m)) . toMemory

runInOut :: [Int64] -> [Int64] -> [Int64]
runInOut m i = reverse . output . run . (\m' -> (i, m')) . toMemory $ m
