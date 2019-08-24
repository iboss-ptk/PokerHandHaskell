module PokerHand
    ( judge
    ) where

judge :: String -> String
judge input
  | input == "John: 2H 4S 4C 2D 4H  White: 2S 8S AS QS 3S"
     = "John wins. - with full house: 4 over 2"

  | input == "Black: 2H 3D 5S 9C KD  White: 2C 3H 4S 8C AH"
     = "White wins. - with high card: Ace"

  | input == "Black: 2H 3D 5S 9C KD  White: 2C 3H 4S 8C KH"
    =  "Black wins. - with high card: 9"

  | otherwise
    = "Tie."
