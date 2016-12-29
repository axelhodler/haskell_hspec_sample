module AdditionTest where

import Addition
import Test.Hspec

test :: IO ()
test = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is 2" $ do
      add 1 1 `shouldBe` 2
