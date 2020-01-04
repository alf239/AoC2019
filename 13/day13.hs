module Main where

import           Control.Monad
import           Control.Monad.State.Strict
import qualified Data.Map.Strict               as M
import           Data.List.Split
import           Intcode
import           System.IO                      ( hFlush
                                                , stdout
                                                )
import           System.Console.ANSI

data Board = Board {
    buffer :: [Int],
    score  :: Value,
    paddle :: Int,
    ball   :: (Int, Int)
}

type Arkanoid = StateT Board IO

block :: Value -> Char
block 0 = ' '
block 1 = '%'
block 2 = '#'
block 3 = '='
block 4 = 'o'

drawTile :: Int -> Int -> Char -> IO ()
drawTile r c ch = do
    setCursorPosition r c
    putStr [ch]
    hFlush stdout


drawScore :: Value -> IO ()
drawScore a = do
    setCursorPosition 3 53
    putStr (show a)
    hFlush stdout

readA :: Arkanoid Value
readA = do
    p        <- gets paddle
    (bx, by) <- gets ball
    let dir = if p > bx then 1 else (if p < bx then -1 else 0)
    return $ if by > 23 then negate dir else dir

writeA :: Value -> Arkanoid ()
writeA a = do
    buf <- gets buffer
    case buf of
        [0, -1] -> do
            liftIO $ drawScore a
            modify $ \s -> s { score = a, buffer = [] }
        [b, c] -> do
            liftIO $ drawTile b c (block $ fromIntegral a)
            case a of
                3         -> modify $ \s -> s { paddle = c, buffer = [] }
                4         -> modify $ \s -> s { ball = (c, b), buffer = [] }
                otherwise -> modify $ \s -> s { buffer = [] }
        otherwise -> modify $ \s -> s { buffer = fromIntegral a : buf }

runGame :: [Value] -> IO Value
runGame m =
    let init      = fromMemory m
        intCode   = execStateT (runProgram readA writeA) init
        ioProgram = execStateT intCode $ Board { buffer = []
                                               , score  = -1
                                               , paddle = -1
                                               , ball   = (-1, -1)
                                               }
    in  score <$> ioProgram

main :: IO ()
main = do
    code <- map read . splitOn "," <$> getContents

    clearScreen
    setCursorPosition 1 45

    putStr "Task 1: "
    let out   = runInOut code []
    let board = M.fromList $ [ ((x, y), z) | [x, y, z] <- chunksOf 3 out ]

    putStr $ show . length . filter ((== 2) . snd) . M.toList $ board
    hFlush stdout

    setCursorPosition 3 45
    putStr "Task 2: "
    let patched = 2 : tail code
    finalScore <- runGame patched

    setCursorPosition 26 0
