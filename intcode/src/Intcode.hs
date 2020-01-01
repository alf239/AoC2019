module Intcode where

import Control.Monad.Loops (whileM)
import Control.Monad.State.Strict
import Data.Functor.Identity (Identity)
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
type IntCodeT = StateT IntState
type IntCode = IntCodeT Identity

currentOp :: IntState -> Value
currentOp s = memory s ! pc s

halted :: IntState -> Bool
halted = (== 99) . currentOp

opcode :: Monad m => IntCodeT m Value
opcode = gets currentOp

fetch :: Monad m => Address -> IntCodeT m Value
fetch addr = do m <- gets memory
                return $ if addr < 0 then error "Negative address"
                                     else Map.findWithDefault 0 addr m

jmp :: Monad m => (Address -> Address) -> IntCodeT m ()
jmp f = modify $ \s -> s { pc = f (pc s)}

store :: Monad m => Address -> Value -> IntCodeT m ()
store a v = modify $ \s -> s { memory = Map.insert a v (memory s) }

addr :: Monad m => Int -> IntCodeT m Value
addr n = do op <- opcode
            b <- gets base
            pc' <- gets pc
            let mode = op `div` 10 ^ (n + 1) `mod` 10
            let param = pc' + fromIntegral n
            case mode of 2 -> (+ b) <$> fetch param
                         1 -> return param
                         0 -> fetch param

setBase :: Monad m => IntCodeT m ()
setBase = do a <- addr 1 >>= fetch
             _ <- modify $ \s -> s { base = a + base s }
             jmp (+ 2)

readIoS :: Monad m => IntCodeT m Value
readIoS = do i <- gets input
             _ <- if null i then error "Input exhausted" 
                            else modify $ \s -> s { input = tail i }
             return $ head i

binaryOp :: Monad m => (Value -> Value -> Value) -> IntCodeT m ()
binaryOp op = do a <- addr 1 >>= fetch
                 b <- addr 2 >>= fetch
                 c <- addr 3
                 _ <- store c (a `op` b)
                 jmp (+ 4)

jif :: Monad m => (Value -> Bool) -> IntCodeT m ()
jif p = do a <- addr 1 >>= fetch
           b <- addr 2 >>= fetch
           let target = if p a then const b else (+ 3)
           jmp target

readInput :: Monad m => IntCodeT m Value -> IntCodeT m ()
readInput rd = do a <- addr 1
                  v <- rd
                  _ <- store a v
                  jmp (+ 2)

writeIoS :: Monad m => Value -> IntCodeT m ()
writeIoS a = modify $ \s -> s { output = a : output s }

writeOutput :: Monad m => (Value -> IntCodeT m ()) -> IntCodeT m ()
writeOutput write = do a <- addr 1 >>= fetch
                       _ <- write a
                       jmp (+ 2)

noop :: Monad m => IntCodeT m ()
noop = jmp (+ 1)

stepRW :: Monad m => IntCodeT m Value -> (Value -> IntCodeT m ()) -> IntCodeT m ()
stepRW rd wr = do op <- (`mod` 100) <$> opcode
                  case op
                    of 0 -> noop
                       1 -> binaryOp (+)
                       2 -> binaryOp (*)
                       3 -> readInput rd
                       4 -> writeOutput wr
                       5 -> jif (/= 0)
                       6 -> jif (== 0)
                       7 -> binaryOp (\a b -> if a < b then 1 else 0)
                       8 -> binaryOp (\a b -> if a == b then 1 else 0)
                       9 -> setBase

step :: IntCode ()
step = stepRW readIoS writeIoS

fromInMemory :: In -> [Value] -> IntState 
fromInMemory i m =
  IntState { input = i, output = [], memory = Map.fromAscList . zip [0..] $ m, pc = 0, base = 0 }

run :: IntCode ()
run = do whileM (gets $ not . halted) step
         return ()

runO :: IntCode ()
runO = do whileM (gets nonBlocking) step
          return ()
       where nonBlocking s = (not . halted $ s) && (null . output $ s)

runI :: IntCode [Value]
runI = do whileM (gets nonBlocking) step
          out <- gets output 
          modify $ \s -> s { output = [] }
          return $ reverse out
       where nonBlocking s    = (not . halted $ s) && (not . exhaustedInput $ s)
             exhaustedInput s = (null . input $ s) && ((== 3) . (`mod` 100) . currentOp $ s)

runMem :: [Value] -> [Value]
runMem = Map.elems . memory . execState run . fromInMemory []

collectOutput :: IntState -> Out
collectOutput s = output s ++ if halted s then [] else collectOutput . execState runO $ s { output = [] }

runInOut :: [Value] -> In -> Out
runInOut m i = collectOutput . fromInMemory i $ m
