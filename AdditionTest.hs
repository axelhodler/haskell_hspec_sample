module AdditionTest where

import Test.Hspec

data Roll = Spare | Gutter | Result Integer deriving (Eq, Show)
roll :: (Integer, Integer) -> Roll
roll (first, second)
  | (first + second) == 10  = Spare
  | otherwise               = Result $ first + second

maxPins = 10

score :: [Roll] -> Integer
score rolls = calculateScore rolls Gutter

type PreviousRoll = Roll

calculateScore :: [Roll] -> PreviousRoll -> Integer
calculateScore [] _ = 0
calculateScore (Spare:xs) Spare = 2 * maxPins + calculateScore xs Spare
calculateScore (Spare:xs) _ = maxPins + calculateScore xs Spare
calculateScore (Result x:xs) Spare = 2 * x + calculateScore xs (Result x)
calculateScore (Result x:xs) _ = x + calculateScore xs (Result x)

test :: IO ()
test = hspec $
  describe "Bowling" $ do
    it "scores 0 on gutter roll" $
      score [roll (0, 0)] `shouldBe` 0
    it "scores 1 on a single hit" $
      score [roll (1, 0)] `shouldBe` 1
    it "scores 3 on hitting 0 then 3" $
      score [roll (0, 3)] `shouldBe` 3
    it "scores 10 on rolling a 1 ten times" $
      score [roll (1, 0), roll(1, 0)] `shouldBe` 2
    it "scores 12 on rolling a spare and a one" $
      score [roll (5, 5), roll(1, 0)] `shouldBe` 12
    it "scores 32 on rolling two spares and a one" $
      score [roll (5, 5), roll(5, 5), roll(1, 0)] `shouldBe` 32
