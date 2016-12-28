module AdditionTest where

import Addition
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is 2" $ do
      add 1 1 `shouldBe` 2
