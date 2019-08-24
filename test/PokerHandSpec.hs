module PokerHandSpec (spec) where

import Test.Hspec
import PokerHand

spec :: Spec
spec =
  describe "PokerHand" $
    describe "judge" $ do
      context "when first player win" $ do
        it "annouces first player as winner: Black with high card 9" $
          judge "Black: 2H 3D 5S 9C KD  White: 2C 3H 4S 8C KH" `shouldBe` "Black wins. - with high card: 9"

        it "annouces first player as winner: John with fullhouse 4 over 2" $
          judge "John: 2H 4S 4C 2D 4H  White: 2S 8S AS QS 3S" `shouldBe` "John wins. - with full house: 4 over 2"

      context "second player wins" $
        it "annouces first player as winner: White with high card Ace" $
          judge "Black: 2H 3D 5S 9C KD  White: 2C 3H 4S 8C AH" `shouldBe` "White wins. - with high card: Ace"

      context "when it's a tie" $
        it "annouces tie" $
          judge "Black: 2H 3D 5S 9C KD  White: 2D 3H 5C 9S KH" `shouldBe` "Tie."

      context "when input is invalid" $
        it "announce that input is not valid" $
          judge "random input" `shouldBe` "`random input` is not a valid input."

