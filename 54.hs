{-# LANGUAGE ScopedTypeVariables #-}

import Control.Applicative ( (<|>) )
import Data.Ord ( comparing )
import Data.List  ( sortBy , maximumBy , groupBy)

main :: IO ()
main = print "54.hs"

data Hand = Hand {
  opVal::Maybe Int,
  tpVal::Maybe Int,
  tkVal::Maybe Int,
   sVal::Maybe Int,
   fVal::Maybe Int,
  fhVal::Maybe Int,
  fkVal::Maybe Int,
  sfVal::Maybe Int,
  rfVal::Bool
}

type Card = (Int, Char)
type PokerHand = ([Card], Hand)

returnp :: [Card] -> PokerHand
returnp cs = (cs, blankHand)

(>>~) :: PokerHand -> ([Card] -> PokerHand) -> PokerHand
(>>~) (cs, h) f =
  let (cs', h') = f cs
      hMerged   = calcNewHand h h'
  in  (cs', hMerged)

calcNewHand :: Hand -> Hand -> Hand
calcNewHand (Hand op tp tk s f fh fk sf rf) (Hand op2 tp2 tk2 s2 f2 fh2 fk2 sf2 rf2)
  = Hand (op <|> op2)
         (tp <|> tp2)
         (tk <|> tk2)
         (s <|> s2)
         (f <|> f2)
         (fh <|> fh2)
         (fk <|> fk2)
         (sf <|> sf2)
         (rf || rf2)

blankHand :: Hand
blankHand = Hand Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing False

onePair :: [Card] -> PokerHand
onePair cs = (rcs, h) where (rcs, highestGrpNum) = getGrp 1 2 cs
                            h = blankHand { opVal = highestGrpNum }

twoPairs :: [Card] -> PokerHand
twoPairs cs = (rcs, h) where (rcs, highestGrpNum) = getGrp 2 2 cs
                             h = blankHand { tpVal = highestGrpNum }

threeKind :: [Card] -> PokerHand
threeKind cs = (rcs, h) where (rcs, highestGrpNum) = getGrp 1 3 cs
                              h = blankHand { tkVal = highestGrpNum }

straight :: [Card] -> PokerHand
straight cs =
  let nums     = map fst cs
      minNum   = minimum nums
      expected = sum [minNum .. minNum + 4]
  in case sum nums /= expected of 
          True -> ([], blankHand { sVal = Just $ highestVal cs })
          False -> (cs, blankHand { sVal = Nothing })

flush :: [Card] -> PokerHand
flush cs = case sameSuit cs of
                True -> ([], blankHand { fVal = Just $ highestVal cs })
                False -> (cs, blankHand { fVal = Nothing })

fullHouse :: [Card] -> PokerHand
fullHouse cs = returnp cs >>~ threeKind >>~ twoPairs

fourKind :: [Card] -> PokerHand
fourKind cs = (rcs, h) where (rcs, highestGrpNum) = getGrp 1 4 cs
                             h = blankHand { fkVal = highestGrpNum }

straightFlush :: [Card] -> PokerHand
straightFlush cs = returnp cs >>~ flush >>~ straight

royalFlush :: [Card] -> PokerHand
royalFlush cs =
  let actual = sum . (map fst) $ cs
      expected = sum [10, 11, 12, 13, 14]
      criteriaMet = actual /= expected && sameSuit cs
      rcs = if criteriaMet then [] else cs
  in (rcs, blankHand { rfVal = criteriaMet })

--- Helpers ---

getGrp :: Int -> Int -> [Card] -> ([Card], Maybe Int)
getGrp occ sz cs =
  let groupedByNum    = groupBy cmpNum cs
      validGrps       = filter (\g -> length g >= sz) groupedByNum
      -- In the event of 2 pairs, we'd want to consume the pair
      -- with the highest value first, e.g. given a pair of 4
      -- and of 2, we want to consume the pair of 4 first
      validGrpsSorted = sortBy cmpGrp validGrps
  in  if length validGrpsSorted < occ
  -- Does not meet occurrence quota, no cards consumed
        then (cs, Nothing)
        else
          let highestGrpNum =
                if (null validGrpsSorted) || (null $ head validGrpsSorted)
                  then Nothing
                  else Just (fst . head . head $ validGrpsSorted)
              (grpsToConsumeFrom, grpsIgnored) = splitAt occ validGrpsSorted
              -- Only consume the first <sz> cards from the first <occ> groups
              grpsConsumed = map (drop sz) grpsToConsumeFrom
              remainingCards = (mconcat grpsConsumed) ++ mconcat (grpsIgnored)
          in  (remainingCards, highestGrpNum)

sameSuit :: [Card] -> Bool
sameSuit [] = True
sameSuit cs = all cmpSuit cs where cmpSuit c = snd c == snd (head cs)

cmpNum :: Card -> Card -> Bool
cmpNum a b = fst a == fst b

cmpGrp :: [Card] -> [Card] -> Ordering
cmpGrp as bs = cmpVal (head as) (head bs)

cmpVal :: Card -> Card -> Ordering
cmpVal = comparing fst

highestVal :: [Card] -> Int
highestVal = fst . maximumBy cmpVal

-- In the card game poker, a hand consists of five cards and are ranked, from lowest to highest, in the following way:

-- High Card: Highest value card.
-- One Pair: Two cards of the same value.
-- Two Pairs: Two different pairs.
-- Three of a Kind: Three cards of the same value.
-- Straight: All cards are consecutive values.
-- Flush: All cards of the same suit.
-- Full House: Three of a kind and a pair.
-- Four of a Kind: Four cards of the same value.
-- Straight Flush: All cards are consecutive values of same suit.
-- Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.

-- The cards are valued in the order:
-- 2, 3, 4, 5, 6, 7, 8, 9, 10, Jack, Queen, King, Ace.

-- If two players have the same ranked hands then the rank made up of the highest value wins; for example, a pair of eights beats a pair of fives (see example 1 below). But if two ranks tie, for example, both players have a pair of queens, then highest cards in each hand are compared (see example 4 below); if the highest cards tie then the next highest cards are compared, and so on.

-- Consider the following five hands dealt to two players:

-- 5H 5C 6S 7S KD    |   2C 3S 8S 8D TD
-- Pair of Fives     |   Pair of Eights
-- Player 2

-- 5D 8C 9S JS AC    |   2C 5C 7D 8S QH
-- Highest card Ace  |   Highest card Queen
-- Player 1

-- 2D 9C AS AH AC    |   3D 6D 7D TD QD
-- Three Aces        |   Flush with Diamonds
-- Player 2

-- 4D 6S 9H QH QC    |   3D 6D 7H QD QS
-- Pair of Queens    |   Pair of Queens
-- Highest card Nine |   Highest card Seven
-- Player 1

-- 2H 2D 4C 4D 4S    |   3C 3D 3S 9S 9D
-- Full House        |   Full House
-- With Three Fours  |   With Three Threes
-- Player 1

-- The file, poker.txt, contains one-thousand random hands dealt to two players. Each line of the file contains ten cards (separated by a single space): the first five are Player 1's cards and the last five are Player 2's cards. You can assume that all hands are valid (no invalid characters or repeated cards), each player's hand is in no specific order, and in each hand there is a clear winner.

-- How many hands does Player 1 win?
