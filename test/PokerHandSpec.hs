module PokerHandSpec (spec) where

import Test.Hspec
import PokerHand

spec :: Spec
spec =
  describe "PokerHand" $
    describe "judge" $ do
      context "first player win" $ do
        it "wins with high card and named Black" $
          judge "Black: 2H 3D 5S 9C KD  White: 2C 3H 4S 8C KH" `shouldBe` "Black wins. - with high card: 9"

        it "wins with full house and named John" $
          judge "John: 2H 4S 4C 2D 4H  White: 2S 8S AS QS 3S" `shouldBe` "John wins. - with full house: 4 over 2"

      context "second player wins" $
        it "wins with high card and named White" $
          judge "Black: 2H 3D 5S 9C KD  White: 2C 3H 4S 8C AH" `shouldBe` "White wins. - with high card: Ace"

      context "ties" $
        it "ties" $
          judge "Black: 2H 3D 5S 9C KD  White: 2D 3H 5C 9S KH" `shouldBe` "Tie."

