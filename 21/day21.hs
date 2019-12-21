module Main where

import Control.Monad
import Control.Monad.Loops
import Data.List (intercalate)
import Data.List.Split
import Data.Set (Set)
import qualified Data.Set as Set
import Intcode
import Data.Char
import Data.Either

runProgram :: [Value] -> [String] -> Either String Value
runProgram mm ss = case o of [result] -> Right result
                             xs       -> Left (map (chr . fromIntegral) xs)
                 where o = runInOut mm p
                       p = map fromIntegral . map ord . intercalate "\n" $ ss

main :: IO ()
main = do input <- map read . splitOn "," <$> getContents

          putStrLn "=== Task 1 ==="
          let result = runProgram input [ "NOT A J"
                                        , "NOT J J"
                                        , "AND B J"
                                        , "AND C J"
                                        , "NOT J J"
                                        , "AND D J"
                                        , "WALK"
                                        , ""
                                        ]
          case result of Right damage -> print damage
                         Left video -> putStrLn video
