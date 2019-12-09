module Intcode where

import qualified Data.Map.Strict as Map
import Data.Int
import Data.Map.Strict ((!), Map)
import Data.List.Split
import Debug.Trace
import Data.List
import Data.Maybe
import Control.Monad.State

type Memory = Map Int64 Int64
type Address = Int64
type In = [Int64]
type Out = [Int64]
type IntState = (In, Out, Memory, Address, Address)


opcode :: State IntState Int64
opcode = do (_, _, m, pc, _) <- get
            return $ m ! pc

fetch :: Int64 -> State IntState Int64
fetch addr = do (_, _, m, _, _) <- get
                return $ if addr < 0 then error("Negative address")
                                     else Map.findWithDefault 0 addr m

jmp :: (Int64 -> Int64) -> State IntState ()
jmp f = do (i, o, m, pc, b) <- get
           put (i, o, m, f pc, b)

store :: Address -> Int64 -> State IntState ()
store a v = do (i, o, m, pc, b) <- get
               put (i, o, Map.insert a v m, pc, b)

addr :: Int64 -> State IntState Int64
addr n = do op <- opcode
            (_, _, m, pc, b) <- get
            let mode = op `div` 10 ^ (n + 1) `mod` 10
            case mode of 2 -> (+ b) <$> fetch (pc + n)
                         1 -> return (pc + n)
                         0 -> fetch (pc + n)

withBase :: Int64 -> State IntState ()
withBase b = do (i, o, m, pc, _) <- get
                put (i, o, m, pc, b)

setBase :: State IntState ()
setBase = do (_, _, _, _, b) <- get
             a <- addr 1 >>= fetch
             withBase $ a + b
             jmp (+ 2)

readIo :: State IntState Int64
readIo = do (i, o, m, pc, b) <- get
            _ <- put (tail i, o, m, pc, b)
            return $ head i

writeIo :: Int64 -> State IntState ()
writeIo x = do (i, o, m, pc, b) <- get
               put (i, x : o, m, pc, b)

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

input :: State IntState ()
input = do a <- addr 1
           v <- readIo
           _ <- store a v
           jmp (+ 2)

output :: State IntState ()
output = do (_, o, _, _, _) <- get
            a <- addr 1 >>= fetch
            _ <- writeIo a
            jmp (+ 2)

stepS :: State IntState ()
stepS = do op <- (`mod` 100) <$> opcode
           case op
             of 1 -> binaryOp (+)
                2 -> binaryOp (*)
                3 -> input
                4 -> output
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
       where initComputer = \im -> case im of (i, m) -> (i, [], m, 0, 0)
             currentOp = \state -> case state of (_, _, m, pc, _) -> m ! pc
             notEnd = (/= 99) . currentOp

runO :: IntState -> IntState
runO = head . dropWhile nonBlocking . iterate step
       where currentOp = \state -> case state of (_, _, m, pc, _) -> m ! pc
             nonBlocking s = notEnd s && (null . output $ s)
             output s = let (_, o, _, _, _) = s in o
             notEnd = (/= 99) . currentOp

toMemory :: [Int64] -> Memory
toMemory = Map.fromList . zip [0..]

memory :: IntState -> Memory
memory (_, _, m, _, _) = m

getOutput :: IntState -> Out
getOutput (_, o, _, _, _) = o

runMem :: [Int64] -> [Int64]
runMem = Map.elems . memory . run . (\m -> ([], m)) . toMemory

runInOut :: [Int64] -> [Int64] -> [Int64]
runInOut m i = reverse . getOutput . run . (\m' -> (i, m')) . toMemory $ m
