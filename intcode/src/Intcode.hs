module Intcode where

import Control.Monad.Loops (whileM)
import Control.Monad.State.Strict
import Control.Monad.Writer.Lazy
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
data IntState = IntState { memory :: Memory
                         , pc :: Address
                         , base :: Address
                         } deriving (Show)

type IntCodeT = StateT IntState
type BasicIC = StateT In (Writer Out)
type IntCode = IntCodeT BasicIC

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

readInput :: Monad m => m Value -> IntCodeT m ()
readInput rd = do a <- addr 1
                  v <- lift rd
                  _ <- store a v
                  jmp (+ 2)

writeOutput :: Monad m => (Value -> m ()) -> IntCodeT m ()
writeOutput write = do a <- addr 1 >>= fetch
                       _ <- lift $ write a
                       jmp (+ 2)

noop :: Monad m => IntCodeT m ()
noop = jmp (+ 1)

stepRW :: Monad m => m Value -> (Value -> m ()) -> IntCodeT m ()
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

runRW :: Monad m => m Value -> (Value -> m ()) -> IntCodeT m ()
runRW rd wr = do whileM (gets $ not . halted) $ stepRW rd wr
                 return ()

readIoS :: BasicIC Value
readIoS = do i <- gets head
             _ <- modify tail
             return i

writeIoS :: Value -> BasicIC ()
writeIoS a = lift $ tell [a]

step :: IntCode ()
step = stepRW readIoS writeIoS

fromMemory :: [Value] -> IntState 
fromMemory m = IntState { memory = Map.fromAscList . zip [0..] $ m, pc = 0, base = 0 }

runMem :: [Value] -> [Value]
runMem m = let init           = fromMemory m
               intCode        = execStateT (runRW readIoS writeIoS) init
               processedInput = evalStateT intCode []
           in Map.elems . memory . fst . runWriter $ processedInput

runInOut :: [Value] -> In -> Out
runInOut m i = let init           = fromMemory m
                   intCode        = runStateT (runRW readIoS writeIoS) init
                   processedInput = runStateT intCode i
               in execWriter processedInput
