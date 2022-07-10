module CalcSpec (spec) where

import           Calc       (calcOne, calcThree, calcTwo)
import           Test.Hspec

spec :: Spec
spec = do
  describe "calcOne" $ do
    it "X" $ do
      calcOne 10 `shouldBe` "X"

    it "0-9" $ do
      calcOne 0 `shouldBe` "0"
      calcOne 1 `shouldBe` "1"
      calcOne 2 `shouldBe` "2"
      calcOne 3 `shouldBe` "3"
      calcOne 4 `shouldBe` "4"
      calcOne 5 `shouldBe` "5"
      calcOne 6 `shouldBe` "6"
      calcOne 7 `shouldBe` "7"
      calcOne 8 `shouldBe` "8"
      calcOne 9 `shouldBe` "9"

  describe "calcTwo" $ do
    it "|X|-|" $ do
      calcTwo 10 0 `shouldBe` "X -"

    it "|0-9|/|" $ do
      calcTwo 0 10 `shouldBe` "0 /"
      calcTwo 1 9 `shouldBe` "1 /"
      calcTwo 9 1 `shouldBe` "9 /"

    it "|0-9|0-9|" $ do
      calcTwo 0 0 `shouldBe` "0 0"
      calcTwo 0 9 `shouldBe` "0 9"
      calcTwo 1 8 `shouldBe` "1 8"
      calcTwo 9 0 `shouldBe` "9 0"

  describe "calcThree" $ do
    it "|X|X|X|" $ do
      calcThree 10 10 10 `shouldBe` "XXX"

    it "|X|X|0-9|" $ do
      calcThree 10 10 0 `shouldBe` "XX0"
      calcThree 10 10 1 `shouldBe` "XX1"
      calcThree 10 10 2 `shouldBe` "XX2"
      calcThree 10 10 3 `shouldBe` "XX3"
      calcThree 10 10 4 `shouldBe` "XX4"
      calcThree 10 10 5 `shouldBe` "XX5"
      calcThree 10 10 6 `shouldBe` "XX6"
      calcThree 10 10 7 `shouldBe` "XX7"
      calcThree 10 10 8 `shouldBe` "XX8"
      calcThree 10 10 9 `shouldBe` "XX9"

    it "|X|0-9|-|" $ do
      calcThree 10 0 0 `shouldBe` "X0 0"
