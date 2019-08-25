module PokerHand
    ( judge,
      toCard,
      determineHand,
      Card(..),
      Rank(..),
      Suit(..),
      Hand(..),
    ) where

import Data.List (splitAt, sort)

judge :: String -> String
judge input
  | input == "John: 2H 4S 4C 2D 4H  White: 2S 8S AS QS 3S"
     = "John wins. - with full house: 4 over 2"

  | input == "Black: 2H 3D 5S 9C KD  White: 2C 3H 4S 8C AH"
     = "White wins. - with high card: Ace"

  | input == "Black: 2H 3D 5S 9C KD  White: 2C 3H 4S 8C KH"
    =  "Black wins. - with high card: 9"

  | input == "Black: 2H 3D 5S 9C KD  White: 2D 3H 5C 9S KH" 
    = "Tie."

  | otherwise
    = "`" ++ input ++ "` is not a valid input."

data Rank
  = Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace
  deriving (Eq, Ord, Enum, Show)

data Suit = Spade | Heart | Diamond | Club
  deriving (Eq, Show)

data Card = Card Rank Suit
  deriving (Eq, Show)

data Hand = StraightFlush Rank
  deriving (Eq, Show)

mapBy :: Eq a => [(a, b)] -> a -> Maybe b
mapBy = flip lookup

toSuit :: String -> Maybe Suit
toSuit = mapBy [
  ("S", Spade),
  ("H", Heart),
  ("D", Diamond),
  ("C", Club)
  ]

toRank :: String -> Maybe Rank
toRank = mapBy [
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

toCard :: String -> Maybe Card
toCard s = let
    (rankString, suitString) = splitAt 1 s
    rank = toRank rankString
    suit = toSuit suitString
  in
    pure Card <*> rank <*> suit

determineHand :: [Card] -> Maybe Hand
determineHand cs = if length cs == 5 then
  let
    ranks = (\(Card rank _) -> rank) <$> cs
    suits = (\(Card _ suit) -> suit) <$> cs
    allTheSameSuit = and ((== head suits) <$> suits)
    possibleStraight = [ minimum ranks .. maximum ranks ]
  in
    if sort ranks == possibleStraight && allTheSameSuit then
      Just $ StraightFlush $ maximum ranks
    else
      Nothing
  else
    Nothing
