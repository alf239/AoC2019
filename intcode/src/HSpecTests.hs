module Main where

import Intcode
import Test.Hspec

main :: IO ()
main = hspec $ do

  describe "Day 2" $ do
    it "can run simple program" $ do
      runMem [1,9,10,3,2,3,11,0,99,30,40,50] `shouldBe` [3500,9,10,70,2,3,11,0,99,30,40,50]
      runMem [1,0,0,0,99] `shouldBe` [2,0,0,0,99]
      runMem [2,3,0,3,99] `shouldBe` [2,3,0,6,99]
      runMem [2,4,4,5,99,0] `shouldBe` [2,4,4,5,99,9801]
      runMem [1,1,1,4,99,5,6,0,99] `shouldBe` [30,1,1,4,2,5,6,0,99]


  describe "Day 5 - position mode" $ do
    it "considers whether the input is equal to 8" $ do
      let equalTo8 = [3,3,1108,-1,8,3,4,3,99]
      runInOut [8] equalTo8 `shouldBe` [1]
      runInOut [9] equalTo8 `shouldBe` [0]
      runInOut [7] equalTo8 `shouldBe` [0]
    it "considers whether the input is less than 8" $ do
      let lessThan8 = [3,9,7,9,10,9,4,9,99,-1,8]
      runInOut [8] lessThan8 `shouldBe` [0]
      runInOut [7] lessThan8 `shouldBe` [1]
    it "checks for zero" $ do
      let isNonZero = [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9]
      runInOut [0] isNonZero `shouldBe` [0]
      runInOut [1] isNonZero `shouldBe` [1]
      runInOut [-1] isNonZero `shouldBe` [1]

  describe "Day 5 - immediate mode" $ do
    it "considers whether the input is equal to 8" $ do
      let equalTo8 = [3,3,1108,-1,8,3,4,3,99]
      runInOut [8] equalTo8 `shouldBe` [1]
      runInOut [9] equalTo8 `shouldBe` [0]
      runInOut [7] equalTo8 `shouldBe` [0]
    it "considers whether the input is less than 8" $ do
      let lessThan8 = [3,3,1107,-1,8,3,4,3,99]
      runInOut [8] lessThan8 `shouldBe` [0]
      runInOut [7] lessThan8 `shouldBe` [1]
    it "checks for zero" $ do
      let isNonZero = [3,3,1105,-1,9,1101,0,0,12,4,12,99,1]
      runInOut [0] isNonZero `shouldBe` [0]
      runInOut [1] isNonZero `shouldBe` [1]
      runInOut [-1] isNonZero `shouldBe` [1]
  
  describe "Day 5 - larger example" $ do
    let compareWith8 = [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
                        1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
                        999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]
    it "outputs 999 if input is less than 8" $ do
      runInOut [-10000] compareWith8 `shouldBe` [999]
      runInOut [0] compareWith8 `shouldBe` [999]
      runInOut [7] compareWith8 `shouldBe` [999]
    it "outputs 1000 if input is 8" $ do
      runInOut [8] compareWith8 `shouldBe` [1000]
    it "outputs 10001 if input is greater than 8" $ do
      runInOut [9] compareWith8 `shouldBe` [1001]
      runInOut [99] compareWith8 `shouldBe` [1001]
      runInOut [1000] compareWith8 `shouldBe` [1001]
