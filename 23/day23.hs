module Main where 

import Intcode
import Data.List.Extra (groupOn)
import Data.List.Split (splitOn, chunksOf)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import Control.Monad.Loops
import Control.Monad.State.Strict

nic :: Value -> [Value] -> IntState
nic i = fromInMemory [i, -1]

add_input :: IntState -> [Value] -> IntState
add_input s i =  s { input = takeWhile (> 0) (input s) ++ i }

step_all :: Map Int IntState -> (Map Int IntState, Maybe [Value])
step_all nics = let step1 = [(k, runState runI s) | (k, s) <- Map.toList nics]
                    msgs = Map.fromListWith (++) [(to, [x, y]) | (_, (out, _)) <- step1, [to, x, y] <- chunksOf 3 out]
                    step1res = Map.fromAscList [(k, add_input s $ Map.findWithDefault [] (fromIntegral k) msgs) | (k, (_, s)) <- step1]
                in (step1res, Map.lookup 255 msgs)

main :: IO ()
main = do code <- map read . splitOn "," <$> getContents

          let nics = Map.fromAscList [((fromIntegral i), nic i code) | i <- [0..49]]

          let steps = iterate (step_all . fst) (nics, Nothing)

          let interesting = snd . head . dropWhile (isNothing . snd) $ steps :: Maybe [Value]

          print interesting



