module AdditionTest where

import Addition
import Test.Hspec
import Test.QuickCheck

test :: IO ()
test = hspec $
  describe "Addition" $ do
    it "1 + 1 is 2" $
      add 1 1 `shouldBe` 2
    it "will always calculate x + 1 as greater than x" $
      property $ \x -> x + 1 > (x :: Int)
