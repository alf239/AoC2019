module Intcode where

import Control.Monad.State
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!), Map)
import Debug.Trace

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
currentOp s = memory s ! pc s

halted :: IntState -> Bool
halted = (== 99) . currentOp

opcode :: State IntState Value
opcode = gets currentOp

fetch :: Address -> State IntState Value
fetch addr = do m <- gets memory
                return $ if addr < 0 then error "Negative address"
                                     else Map.findWithDefault 0 addr m

jmp :: (Address -> Address) -> State IntState ()
jmp f = modify $ \s -> s { pc = f (pc s)}

store :: Address -> Value -> State IntState ()
store a v = modify $ \s -> s { memory = Map.insert a v (memory s) }

addr :: Int -> State IntState Value
addr n = do op <- opcode
            b <- gets base
            pc' <- gets pc
            let mode = op `div` 10 ^ (n + 1) `mod` 10
            let param = pc' + fromIntegral n
            case mode of 2 -> (+ b) <$> fetch param
                         1 -> return param
                         0 -> fetch param

setBase :: State IntState ()
setBase = do a <- addr 1 >>= fetch
             _ <- modify $ \s -> s { base = a + base s }
             jmp (+ 2)

readIo :: State IntState Value
readIo = do i <- gets input
            _ <- if null i then error "Input exhausted" 
                           else modify $ \s -> s { input = tail i }
            return $ head i

writeIo :: Value -> State IntState ()
writeIo x = modify $ \s -> s { output = x : output s }

binaryOp :: (Value -> Value -> Value) -> State IntState ()
binaryOp op = do a <- addr 1 >>= fetch
                 b <- addr 2 >>= fetch
                 c <- addr 3
                 _ <- store c (a `op` b)
                 jmp (+ 4)

jif :: (Value -> Bool) -> State IntState ()
jif p = do a <- addr 1 >>= fetch
           b <- addr 2 >>= fetch
           let target = if p a then const b else (+ 3)
           jmp target

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

noop :: State IntState ()
noop = jmp (+ 1)

stepS :: State IntState ()
stepS = do op <- (`mod` 100) <$> opcode
           case op
             of 0 -> noop
                1 -> binaryOp (+)
                2 -> binaryOp (*)
                3 -> readInput
                4 -> writeOutput
                5 -> jif (/= 0)
                6 -> jif (== 0)
                7 -> binaryOp (\a b -> if a < b then 1 else 0)
                8 -> binaryOp (\a b -> if a == b then 1 else 0)
                9 -> setBase

step :: IntState -> IntState 
step = execState stepS

fromInMemory :: In -> [Value] -> IntState 
fromInMemory i m =
  IntState { input = i, output = [], memory = Map.fromAscList . zip [0..] $ m, pc = 0, base = 0 }

run :: IntState -> IntState 
run = head . dropWhile (not . halted) . iterate step

runO :: IntState -> IntState
runO = head . dropWhile nonBlocking . iterate step
       where nonBlocking s = (not . halted $ s) && (null . output $ s)

runMem :: [Value] -> [Value]
runMem = Map.elems . memory . run . fromInMemory []

collectOutput :: IntState -> Out
collectOutput s = output s ++ if halted s then [] else collectOutput . runO $ s { output = [] }

runInOut :: [Value] -> In -> Out
runInOut m i = collectOutput . fromInMemory i $ m
