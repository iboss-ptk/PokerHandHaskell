module PokerHandSpec (spec) where

import Test.Hspec
import Control.Monad (forM_)
import PokerHand

spec :: Spec
spec =
  describe "PokerHand" $ do
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

    describe "toCard" $
      let
        rankMapping = [
          ("A", Ace),
          ("2", Two),
          ("3", Three),
          ("4", Four),
          ("5", Five),
          ("6", Six),
          ("7", Seven),
          ("8", Eight),
          ("9", Nine),
          ("T", Ten),
          ("J", Jack),
          ("Q", Queen),
          ("K", King)
          ]
      in do
        context "when rank is valid and ends with H" $
          it "returns card in rank with heart suit" $ forM_ rankMapping $
            \(rankString, rank) ->
              toCard (rankString ++ "H") `shouldBe` (Just $ Card rank Heart)

        context "when rank is valid and ends with S" $
          it "returns card in rank with spade suit" $ forM_ rankMapping $
            \(rankString, rank) ->
              toCard (rankString ++ "S") `shouldBe` (Just $ Card rank Spade)

        context "when rank is valid and ends with C" $
          it "returns card in rank with club suit" $ forM_ rankMapping $
            \(rankString, rank) ->
              toCard (rankString ++ "C") `shouldBe` (Just $ Card rank Club)

        context "when rank is valid and ends with D" $
          it "returns card in rank with diamond suit" $ forM_ rankMapping $
            \(rankString, rank) ->
              toCard (rankString ++ "D") `shouldBe` (Just $ Card rank Diamond)

        context "when the string does match neither rank or suit" $
          it "returns Nothing" $ do
              toCard "" `shouldBe` Nothing
              toCard " " `shouldBe` Nothing
              toCard "A" `shouldBe` Nothing
              toCard "AHA" `shouldBe` Nothing
              toCard "4X" `shouldBe` Nothing
              toCard "garbage" `shouldBe` Nothing
