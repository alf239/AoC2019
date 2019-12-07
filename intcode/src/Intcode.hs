module Intcode where

import Data.Array
import Data.List.Split
import Debug.Trace
import Data.List
import Data.Maybe
import Control.Monad.State

type Memory = Array Int Int
type Address = Int
type In = [Int]
type Out = [Int]
type IntState = (In, Out, Memory, Address)


opcode :: State IntState Int
opcode = do (_, _, m, pc) <- get
            return $ m ! pc

fetch :: Int -> State IntState Int
fetch addr = do (_, _, m, _) <- get
                return $ m ! addr

jmp :: (Int -> Int) -> State IntState ()
jmp f = do (i, o, m, pc) <- get
           put (i, o, m, f pc)

store :: Address -> Int -> State IntState ()
store a v = do (i, o, m, pc) <- get
               put (i, o, m//[(a, v)], pc)

addr :: Int -> State IntState Int
addr n = do op <- opcode
            (_, _, m, pc) <- get
            let mode = op `div` 10 ^ (n + 1) `mod` 10
            if mode == 1 then return (pc + n)
                         else fetch (pc + n)

readIo :: State IntState Int
readIo = do (i, o, m, pc) <- get
            _ <- put (tail i, o, m, pc)
            return $ head i

writeIo :: Int -> State IntState ()
writeIo x = do (i, o, m, pc) <- get
               put (i, x : o, m, pc)

binaryOp :: (Int -> Int -> Int) -> State IntState ()
binaryOp op = do a <- addr 1 >>= fetch
                 b <- addr 2 >>= fetch
                 c <- addr 3
                 let v = a `op` b
                 _ <- store c v
                 jmp (+ 4)

jif :: (Int -> Bool) -> State IntState ()
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
output = do (i, o, m, pc) <- get
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
                7 -> binaryOp (\a b -> fromEnum $ a < b)
                8 -> binaryOp (\a b -> fromEnum $ a == b)
                99 -> return ()

step :: IntState -> IntState 
step = execState stepS

run :: (In, Memory) -> IntState 
run = head . dropWhile notEnd . iterate step . initComputer
       where initComputer = \im -> case im of (i, m) -> (i, [], m, 0)
             currentOp = \state -> case state of (_, _, m, pc) -> m ! pc
             notEnd = (/= 99) . currentOp

runO :: IntState -> IntState
runO = head . dropWhile nonBlocking . iterate step
       where currentOp = \state -> case state of (_, _, m, pc) -> m ! pc
             nonBlocking s = notEnd s && (null . output $ s)
             output s = let (_, o, _, _) = s in o
             notEnd = (/= 99) . currentOp

toMemory :: [Int] -> Memory
toMemory as = listArray (0, length as - 1) as

memory :: IntState -> Memory
memory (_, _, m, _) = m

runMem :: [Int] -> [Int]
runMem = elems . memory . run . (\m -> ([], m)) . toMemory