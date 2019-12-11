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

roboStep :: State Painter ()
roboStep = do m <- gets hull
              d <- gets dir
              p <- gets pos
              ic <- gets program
              let ic' = execState runO $ ic { input = [ Map.findWithDefault 0 p m ] }
              modify $ \s -> s { program = ic' { output = [] } }
              if halted ic'
                then traceShow "halted 1" $ return ()
                else 
                  let [c]  = output ic'
                      ic'' = execState runO $ ic' { output = [] }
                      [dd] = output ic'' 
                      d'   = if dd == 0 then prev d else next d
                   in put $ Painter { hull = Map.insert p c m, dir = d', pos = move d' p, program = ic'' { output = [] } }

robot1' :: State Painter (Map Position Value)
robot1' = do whileM (gets $ not . halted . program) roboStep
             gets hull

robot1 :: Painter -> Int
robot1 = Map.size . evalState robot1'

main :: IO ()
main = do input <- map read . splitOn "," <$> getContents
          let program = fromInMemory [] input
          let painter = Painter Map.empty North (0, 0) program

          putStrLn "=== Task 1 ==="
          print $ robot1 painter 

          let painter2 = Painter (Map.fromList [((0, 0), 1)]) North (0, 0) program
          putStrLn "=== Task 2 ==="
          let result = evalState robot1' painter2
          let picture = reverse [reverse [if Map.findWithDefault 0 (x, y) result == 1 then '#' else ' ' | x <- [-42..0]] | y <- [-20..0]]
          putStrLn . unlines $ picture


