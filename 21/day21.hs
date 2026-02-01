module Main where

import Control.Monad
import Control.Monad.Loops
import Data.List (intercalate)
import Data.List.Split
import Data.Set (Set)
import qualified Data.Set as Set
import Intcode hiding (runProgram)
import Data.Char
import Data.Either
import Debug.Trace

runProgram :: [Value] -> [String] -> Either String Value
runProgram mm ss = if last o > 255 then Right (last o)
                                   else Left (asString o)
                 where asString = map (chr . fromIntegral)
                       o = runInOut mm p
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

          putStrLn "=== Task 2 ==="
          let result2 = runProgram input [ "NOT A J"
                                         , "NOT J J"
                                         , "AND B J"
                                         , "AND C J"
                                         , "NOT J J" -- !(A && B && C)
                                         , "NOT H T"
                                         , "NOT T T"
                                         , "OR E T" -- (H || E)
                                         , "AND T J"
                                         , "AND D J"
                                         , "RUN"
                                         , ""
                                         ]
          case result2 of Right damage -> print damage
                          Left video -> putStrLn video
