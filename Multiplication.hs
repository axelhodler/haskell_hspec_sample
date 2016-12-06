module Multiplication where

import Test.Hspec

multiply :: Int -> Int -> Int
multiply x y
  | y >= 1    = x + multiply (y-1) x
  | otherwise = 0

main :: IO ()
main = hspec $ do
  describe "Multiplication" $ do
    it "multiplies 2 * 2 to 4" $ do
      multiply 2 2 `shouldBe` 4
