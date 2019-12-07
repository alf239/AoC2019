module Main where

import Intcode
import Test.Hspec

main :: IO ()
main = hspec $ do

  describe "Validate day 2" $ do
    it "can run simple program" $ do
      runMem [1,9,10,3,2,3,11,0,99,30,40,50] `shouldBe` [3500,9,10,70,2,3,11,0,99,30,40,50]
      runMem [1,0,0,0,99] `shouldBe` [2,0,0,0,99]
      runMem [2,3,0,3,99] `shouldBe` [2,3,0,6,99]
      runMem [2,4,4,5,99,0] `shouldBe` [2,4,4,5,99,9801]
      runMem [1,1,1,4,99,5,6,0,99] `shouldBe` [30,1,1,4,2,5,6,0,99]
