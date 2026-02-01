module Main where

import Control.Monad.State.Strict
import Data.List.Split
import Intcode
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

data Dir = North | East | South | West deriving (Show, Eq, Ord, Bounded, Enum)
type Position = (Int, Int)

data PainterState = PainterState
    { hull   :: Map Position Value
    , dir    :: Dir
    , pos    :: Position
    , buffer :: [Value]  -- buffer for output pairs [color, turn]
    }

type Painter = StateT PainterState IO

next :: (Eq a, Enum a, Bounded a) => a -> a
next e | e == maxBound = minBound
       | otherwise = succ e

prev :: (Eq a, Enum a, Bounded a) => a -> a
prev e | e == minBound = maxBound
       | otherwise = pred e

move :: Dir -> Position -> Position
move North (x, y) = (x, y + 1)
move West  (x, y) = (x + 1, y)
move South (x, y) = (x, y - 1)
move East  (x, y) = (x - 1, y)

-- Read current panel color
readP :: Painter Value
readP = do
    p <- gets pos
    h <- gets hull
    return $ Map.findWithDefault 0 p h

-- Write output: buffer until we have [color, turn], then paint & move
writeP :: Value -> Painter ()
writeP v = do
    buf <- gets buffer
    case buf of
        [color] -> do
            -- We have both: color was buffered, v is the turn direction
            let turn = v
            -- Paint current position
            p <- gets pos
            modify $ \s -> s { hull = Map.insert p color (hull s) }
            -- Turn
            d <- gets dir
            let newDir = if turn == 0 then prev d else next d
            modify $ \s -> s { dir = newDir }
            -- Move forward
            let newPos = move newDir p
            modify $ \s -> s { pos = newPos, buffer = [] }
        [] -> do
            -- Buffer the color, wait for turn direction
            modify $ \s -> s { buffer = [v] }
        _ -> error "Unexpected buffer state"

runPainter :: Map Position Value -> [Value] -> IO (Map Position Value)
runPainter initial mem = do
    let initialState = PainterState
            { hull = initial
            , dir = North
            , pos = (0, 0)
            , buffer = []
            }
    let intCode = execIntcodeT readP writeP mem
    finalState <- execStateT intCode initialState
    return $ hull finalState

main :: IO ()
main = do
    input <- map read . splitOn "," <$> getContents

    putStrLn "=== Task 1 ==="
    result1 <- runPainter Map.empty input
    print $ Map.size result1

    putStrLn "=== Task 2 ==="
    result2 <- runPainter (Map.fromList [((0, 0), 1)]) input
    let picture = reverse
            [ reverse [if Map.findWithDefault 0 (x, y) result2 == 1 then '#' else ' ' | x <- [-42..0]]
            | y <- [-20..0]
            ]
    putStrLn . unlines $ picture
