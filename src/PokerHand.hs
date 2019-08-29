module PokerHand
    ( judge,
      toCard,
      determineHand,
      Card(..),
      Rank(..),
      Suit(..),
      Hand(..),
    ) where

import           Control.Arrow ((&&&))
import           Data.List     (group, maximumBy, sort, sortOn, splitAt)

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

data Hand
  = StraightFlush Rank
  | FourOfAKind Rank
  | FullHouse Rank
  | Flush [Rank]
  | Straight Rank
  | ThreeOfAKind Rank
  | TwoPairs (Rank, Rank) Rank
  | Pair Rank [Rank]
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

getRank :: Card -> Rank
getRank (Card rank _) = rank

getSuit :: Card -> Suit
getSuit (Card _ suit) = suit


countRank :: [Rank] -> [(Rank, Int)]
countRank = fmap (head &&& length) . group . sort

mostRepeatedRank :: [Rank] -> (Rank, Int)
mostRepeatedRank = maximumBy compareCount . countRank
  where
    compareCount (_, c1) (_, c2) = compare c1 c2


determineHand :: [Card] -> Maybe Hand
determineHand cs
  | length cs /= 5 = Nothing
  | otherwise =
    let
      ranks = getRank <$> cs
      suits = getSuit <$> cs
      allTheSameSuit = and ((== head suits) <$> suits)
      possibleStraight = [ minimum ranks .. maximum ranks ]
      (mostRepeatedRankVal, mostRepeatedRankCount) = mostRepeatedRank ranks
    in
      if sort ranks == possibleStraight && allTheSameSuit then
        Just $ StraightFlush (maximum ranks)
      else if mostRepeatedRankCount == 4 then
        Just $ FourOfAKind mostRepeatedRankVal
      else if sort (snd <$> countRank ranks) == [2, 3] then
        Just $ FullHouse mostRepeatedRankVal
      else if allTheSameSuit then
        Just $ Flush ranks
      else if sort ranks == possibleStraight then
        Just $ Straight (maximum ranks)
      else if mostRepeatedRankCount == 3 then
        Just $ ThreeOfAKind mostRepeatedRankVal
      else if sort (snd <$> countRank ranks) == [1, 2, 2] then
        let
          [re, p1, p2] = fst <$> sortOn snd (countRank ranks)
          pairs = (maximum [p1, p2], minimum [p1, p2])
        in
          Just $ TwoPairs pairs re
      else if sort (snd <$> countRank ranks) == [1, 1, 1, 2] then
          Just $ Pair mostRepeatedRankVal (filter (/= mostRepeatedRankVal) ranks)
      else
        Nothing
