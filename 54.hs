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

import Data.Ord ( comparing )
import Data.List ( maximumBy, groupBy )

data HighCard      = HC { hcVal::Int }
data OnePair       = OP { opVal::Int }
data TwoPairs      = TP { tpVal::Int }
data ThreeKind     = TK { tkVal::Int }
data Straight      = S  {  sVal::Int }
data Flush         = F  {  fVal::Int }
data FullHouse     = FH { fhVal::Int }
data FourKind      = FK { fkVal::Int }
data StraightFlush = SF { sfVal::Int }
data RoyalFlush    = RF { rfVal::Int }

type Card = (Int, Char)

main :: IO ()
main = print "54.hs"

-- solve :: Int 

highestCard :: [Card] -> Int
highestCard = fst . maximumBy cmpVal

onePair :: [Card] -> Maybe Int
onePair = getGrp 1 2

twoPairs :: [Card] -> Maybe Int
twoPairs = getGrp 2 2

threeKind :: [Card] -> Maybe Int
threeKind = getGrp 1 3

flush :: [Card] -> Maybe ()
flush cs = if (not . sameSuit) cs
             then Nothing
             else Just ()

-- Return highest num value
straight :: [Card] -> Maybe Int
straight cs = let nums = map fst cs
                  minNum = minimum nums
                  expected = sum [minNum .. minNum + 4]
              in  if sum nums /= expected
                    then Nothing
                    else Just $ maximum nums

fullHouse :: [Card] -> Maybe (Int, Int)
fullHouse cs = pure (,) <*> threeKind cs <*> onePair cs

fourKind :: [Card] -> Maybe Int
fourKind = getGrp 1 4

straightFlush :: [Card] -> Maybe Int
straightFlush cs = flush cs >> straight cs

-- TODO:
royalFlush :: [Card] -> Maybe ()
royalFlush cs = let nums = map fst cs
                    expected = 60 -- Expect 10 - Ace (J = 11, Q = 12, K = 13, A = 14)
                    numsSatisfied = if sum nums /= expected
                                      then Nothing
                                      else Just ()
                in flush cs >> numsSatisfied

--- helpers --

-- If no group found, returns Nothing
-- Otherwise returns the highest number in any of the groups
getGrp :: Int -> Int -> [Card] -> Maybe Int
getGrp occ sz cs = if length (groupByNum cs) < occ
                     then Nothing
                     else Just (maxNum . filter isLongEnough $ groupByNum cs)
                     where groupByNum = groupBy cmpNum
                           isLongEnough cs' = length cs' >= sz
                           maxNum = fst . head . maximumBy cmpGrp

sameSuit :: [Card] -> Bool
sameSuit [] = True
sameSuit cs = all cmpSuit cs
              where cmpSuit c = snd c == snd (head cs)

cmpNum :: Card -> Card -> Bool
cmpNum a b = fst a == fst b

cmpGrp :: [Card] -> [Card] -> Ordering
cmpGrp as bs = cmpVal (head as) (head bs)

cmpVal :: Card -> Card -> Ordering
cmpVal = comparing fst