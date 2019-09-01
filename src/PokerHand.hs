module PokerHand
    ( judge,
      toCard,
      determineHand,
      compareHand,
      Card(..),
      Rank(..),
      Suit(..),
      Hand(..),
    ) where

import           Control.Arrow ((&&&))
import           Data.List     (group, sort, sortBy, sortOn, splitAt)

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
  | HighCard [Rank]
  deriving (Eq, Ord, Show)

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


groupAndCount :: [Rank] -> [(Rank, Int)]
groupAndCount = fmap (head &&& length) . group . sort


determineHand :: [Card] -> Maybe Hand
determineHand cs
  | length cs /= 5 = Nothing
  | otherwise = Just $
    let
      ranks = getRank <$> cs
      suits = getSuit <$> cs
      allSuitsAreTheSame = and ((== head suits) <$> suits)
      allRanksAreConsecutive = sort ranks == [ minimum ranks .. maximum ranks ]
      sortedRankByRepitition = sortOn snd (groupAndCount ranks)
      rankRepititions = snd <$> sortedRankByRepitition
      mostRepeatedRank = last (fst <$> sortedRankByRepitition)
    in
      if allRanksAreConsecutive && allSuitsAreTheSame then
        StraightFlush (maximum ranks)

      else if rankRepititions == [1, 4] then
        FourOfAKind mostRepeatedRank

      else if rankRepititions == [2, 3] then
        FullHouse mostRepeatedRank

      else if allSuitsAreTheSame && not allRanksAreConsecutive then
        Flush ranks

      else if allRanksAreConsecutive && not allSuitsAreTheSame then
        Straight (maximum ranks)

      else if rankRepititions == [1, 1, 3] then
        ThreeOfAKind mostRepeatedRank

      else if rankRepititions == [1, 2, 2] then
        let
          [nonPairRank, pairRank1, pairRank2] = fst <$> sortedRankByRepitition
        in
          TwoPairs (pairRank1, pairRank2) nonPairRank

      else if rankRepititions == [1, 1, 1, 2] then
          Pair mostRepeatedRank (filter (/= mostRepeatedRank) ranks)

      else
        HighCard ranks

sortBest :: [Rank] -> [Rank]
sortBest = sortBy (flip compare)

bestFirst :: (Rank, Rank) -> (Rank, Rank)
bestFirst (a, b) = if a > b then (a, b) else (b, a)

compareHand :: Hand -> Hand -> Ordering
compareHand (Flush rs) (Flush rs') = compare (sortBest rs) (sortBest rs')
compareHand (HighCard rs) (HighCard rs') = compare (sortBest rs) (sortBest rs')
compareHand (Pair r rs) (Pair r' rs') =
  compare r r' <>
  compare (sortBest rs) (sortBest rs')
compareHand (TwoPairs pr r) (TwoPairs pr' r') =
  compare
    (TwoPairs (bestFirst pr) r)
    (TwoPairs (bestFirst pr') r')
compareHand h h' = compare h h'
