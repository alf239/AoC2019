module Main where

import Control.Monad.Loops (whileM)
import Control.Monad.State.Strict
import Debug.Trace
import Data.List.Split
import Intcode
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!), Map)

data Dir = North | East | South | West deriving (Show, Eq, Ord, Bounded, Enum)
type Position = (Int, Int)
data Painter = Painter { hull    :: (Map Position Value)
                       , dir     :: Dir
                       , pos     :: Position
                       , program :: IntState
                       }

next :: (Eq a, Enum a, Bounded a) => a -> a
next e | e == maxBound = minBound
       | otherwise = succ e

prev :: (Eq a, Enum a, Bounded a) => a -> a
prev e | e == minBound = maxBound
       | otherwise = pred e

move :: Dir -> (Int, Int) -> Position
move North (x, y) = (x, y + 1)
move West  (x, y) = (x + 1, y)
move South (x, y) = (x, y - 1)
move East  (x, y) = (x - 1, y)

sense :: Painter -> Value
sense p = Map.findWithDefault 0 (pos p) (hull p)

executeUntilBlocked :: [Value] -> State Painter [Value]
executeUntilBlocked i = do ic <- gets program
                           let (out, ic') = runState runI $ ic { input = i }
                           modify $ \s -> s { program = ic' }
                           return out

turn :: (Dir -> Dir) -> State Painter ()
turn t = modify $ \s -> s { dir = t . dir $ s }

paint :: Value -> State Painter ()
paint c = modify $ \s -> s { hull = Map.insert (pos s) c (hull s) }

stepForward :: State Painter ()
stepForward = modify $ \s -> s { pos = move (dir s) (pos s) }

roboStep :: State Painter ()
roboStep = do s <- gets sense
              out <- executeUntilBlocked [s]
              let [c, dd] = out
              _ <- paint c
              _ <- turn $ if dd == 0 then prev else next
              stepForward 

paintHull :: State Painter (Map Position Value)
paintHull = do whileM (gets $ not . halted . program) roboStep
               gets hull

main :: IO ()
main = do input <- map read . splitOn "," <$> getContents
          let program = fromInMemory [] input

          putStrLn "=== Task 1 ==="
          let painter = Painter Map.empty North (0, 0) program
          print . Map.size $ evalState paintHull painter 

          putStrLn "=== Task 2 ==="
          let painter2 = Painter (Map.fromList [((0, 0), 1)]) North (0, 0) program
          let result = evalState paintHull painter2
          let picture = reverse [reverse [if Map.findWithDefault 0 (x, y) result == 1 then '#' else ' ' | x <- [-42..0]] | y <- [-20..0]]
          putStrLn . unlines $ picture


