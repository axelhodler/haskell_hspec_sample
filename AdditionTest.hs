module AdditionTest where

import Test.Hspec

data Roll = Strike | Spare | Frame Integer Integer deriving (Eq, Show)
data OpenBonus = NoBonus | One | Two | Three

roll :: (Integer, Integer) -> Roll
roll (first, second)
  | first == 10             = Strike
  | (first + second) == 10  = Spare
  | otherwise               = Frame first second

maxPins :: Integer
maxPins = 10

score :: [Roll] -> Integer
score rolls = calculateScore rolls NoBonus

calculateScore :: [Roll] -> OpenBonus -> Integer
calculateScore [] _ = 0
calculateScore (Strike:xs) Two = 2 * maxPins + calculateScore xs Three
calculateScore (Strike:xs) One = 2 * maxPins + calculateScore xs Two
calculateScore (Strike:xs) NoBonus = maxPins + calculateScore xs Two
calculateScore (Spare:xs) Two = 2 * maxPins + calculateScore xs One
calculateScore (Spare:xs) One = 2 * maxPins + calculateScore xs One
calculateScore (Spare:xs) NoBonus = maxPins + calculateScore xs One
calculateScore (Frame x y:xs) Three = 3 * x + 2 * y + calculateScore xs NoBonus
calculateScore (Frame x y:xs) Two = 2 * x + 2 * y + calculateScore xs NoBonus
calculateScore (Frame x y:xs) One = 2 * x + y + calculateScore xs NoBonus
calculateScore (Frame x y:xs) NoBonus = x + y + calculateScore xs NoBonus

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
    it "scores 37 on rolling two strikes and a 1 2 frame" $
      score [roll (10, 0), roll(10, 0), roll(1, 2)] `shouldBe` 37
