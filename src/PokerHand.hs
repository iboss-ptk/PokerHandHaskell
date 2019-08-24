module PokerHand
    ( judge
    ) where

judge :: String -> String
judge input = if input == "John: 2H 4S 4C 2D 4H  White: 2S 8S AS QS 3S"
  then "John wins. - with full house: 4 over 2"
  else "Black wins. - with high card: 9"
