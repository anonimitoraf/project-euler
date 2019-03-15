{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Monad                  ( liftM2 )
import           Control.Applicative            ( (<|>) )
import           Data.Ord                       ( comparing )
import           Data.List                      ( sort
                                                , sortBy
                                                , maximumBy
                                                , groupBy
                                                , partition
                                                )
import           Data.Char                      ( intToDigit
                                                , digitToInt
                                                )
import           Control.Arrow                  ( (***) )

data Hand = Hand {
  hcVal::Int,
  opVal::Maybe Int,
  tpVal::Maybe (Int, Int),
  tkVal::Maybe Int,
   sVal::Maybe Int,
   fVal::Maybe Int,
  fhVal::Maybe (Int, Int),
  fkVal::Maybe Int,
  sfVal::Maybe Int,
  rfVal::Bool
}

type NonNormalizedCard = (Char, Char)
type Card = (Int, Char)
type PokerHand = ([Card], Hand)
type Versus a = (a, a)

data Winner = Player1 | Player2
instance Show Winner where
  show Player1 = "Player 1"
  show Player2 = "Player 2"

handCardCount = 5

main :: IO ()
main = do input <- readFile "p054_poker.txt"
          let nonNormalizedCards = inputToCards input
              cards :: [Versus [Card]] = map (map normalizeCard *** map normalizeCard) nonNormalizedCards
              cardsToHand = snd . evalCards
              hands :: [Versus Hand] =  map (cardsToHand *** cardsToHand) cards
              winners :: [Winner] = map (uncurry evalHands) hands
          print winners

toCardDraw :: [String] -> [NonNormalizedCard]
toCardDraw = map toCard where toCard s = (head s, s !! 1)

inputToCards :: String -> [Versus [NonNormalizedCard]]
inputToCards s = let cardStrGrps :: [[String]] = chunksOf (handCardCount * 2) . words $ s
                     player1Cards :: [[NonNormalizedCard]] = map (toCardDraw . take handCardCount) cardStrGrps
                     player2Cards :: [[NonNormalizedCard]] = map (toCardDraw . take handCardCount) cardStrGrps
                 in zip player1Cards player2Cards

normalizeCard :: NonNormalizedCard -> Card
normalizeCard ('T', s) = (10, s)
normalizeCard ('J', s) = (11, s)
normalizeCard ('Q', s) = (12, s)
normalizeCard ('K', s) = (13, s)
normalizeCard ('A', s) = (14, s)
normalizeCard (i, s) = (digitToInt i, s)

returnp :: [Card] -> PokerHand
returnp cs = (cs, blankHand)

(>>~) :: PokerHand -> ([Card] -> PokerHand) -> PokerHand
(>>~) (cs, h) f =
  let (cs', h') = f cs
      hMerged   = calcNewHand h h'
  in  (cs', hMerged)

calcNewHand :: Hand -> Hand -> Hand
calcNewHand (Hand hc op tp tk s f fh fk sf rf)
            (Hand hc2 op2 tp2 tk2 s2 f2 fh2 fk2 sf2 rf2)
            = Hand (if hc < hc2 then hc2 else hc)
                   (op <|> op2)
                   (tp <|> tp2)
                   (tk <|> tk2)
                   (s <|> s2)
                   (f <|> f2)
                   (fh <|> fh2)
                   (fk <|> fk2)
                   (sf <|> sf2)
                   (rf || rf2)

evalHands :: Hand -> Hand -> Winner
evalHands _ _ = Player1

evalCards :: [Card] -> PokerHand
evalCards cs = returnp cs >>~ royalFlush
                          >>~ straightFlush
                          >>~ fourKind
                          >>~ fullHouse
                          >>~ flush
                          >>~ straight
                          >>~ threeKind
                          >>~ twoPairs
                          >>~ onePair
                          >>~ highestCard

blankHand :: Hand
blankHand = Hand (-1) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing False

onePair :: [Card] -> PokerHand
onePair cs = (cs, blankHand { opVal = getGrp 1 2 cs })

twoPairs :: [Card] -> PokerHand
twoPairs cs = let maybeFstGrpNum = getGrp 1 2 cs
                  updatedCs = case maybeFstGrpNum of
                                   Nothing -> cs
                                   Just fstGrpNum -> removeFirstNBy (\c -> fst c == fstGrpNum) 2 cs
                  maybeSndGrpNum = getGrp 1 2 updatedCs
              in (cs, blankHand { tpVal = liftM2 (,) maybeFstGrpNum maybeSndGrpNum })

threeKind :: [Card] -> PokerHand
threeKind cs = (cs, blankHand { tkVal = getGrp 1 3 cs })

straight :: [Card] -> PokerHand
straight [] = ([], blankHand)
straight cs =
  let nums = map fst cs
  in case isSequential nums of
          True -> (cs, blankHand { sVal = Just $ highestVal cs })
          False -> (cs, blankHand)

flush :: [Card] -> PokerHand
flush [] = ([], blankHand)
flush cs = case sameSuit cs of
                True -> (cs, blankHand { fVal = Just $ highestVal cs })
                False -> (cs, blankHand)

fullHouse :: [Card] -> PokerHand
fullHouse cs = let (cs', Hand { tkVal = tk, opVal = op }) = returnp cs >>~ threeKind >>~ onePair
               in (cs', blankHand { fhVal = (,) <$> tk <*> op })

fourKind :: [Card] -> PokerHand
fourKind cs = (cs, blankHand { fkVal = getGrp 1 4 cs })

straightFlush :: [Card] -> PokerHand
straightFlush cs = let (cs', Hand { fVal = f, sVal = s }) = returnp cs >>~ flush >>~ straight
                   in (cs', blankHand { sfVal = s >> f })

royalFlush :: [Card] -> PokerHand
royalFlush cs =
  let nums = map fst cs
      minNum = minimum nums
      criteriaMet = minNum == 10 && isSequential nums && sameSuit cs
  in (cs, blankHand { rfVal = criteriaMet })

highestCard :: [Card] -> PokerHand
highestCard cs = (cs, blankHand { hcVal = highestVal cs })

--- Helpers ---

getGrp :: Int -> Int -> [Card] -> Maybe Int
getGrp occ sz cs =
  let revSortedCs = sortBy (flip cmpNumOrd) cs
      groupedByNum    = groupBy cmpNum revSortedCs
      -- validGrps is sorted desc (due to revSortedCs being sorted already)
      validGrps = filter (\g -> length g >= sz) groupedByNum
  in  if length validGrps < occ
        then Nothing
        else
          let highestGrpNum =
                if (null validGrps) || (null $ head validGrps)
                  then Nothing
                  else Just (fst . head . head $ validGrps)
          in  highestGrpNum

sameSuit :: [Card] -> Bool
sameSuit [] = True
sameSuit cs = all cmpSuit cs where cmpSuit c = snd c == snd (head cs)

cmpNum :: Card -> Card -> Bool
cmpNum a b = fst a == fst b

cmpGrp :: [Card] -> [Card] -> Ordering
cmpGrp as bs = cmpNumOrd (head as) (head bs)

cmpNumOrd :: Card -> Card -> Ordering
cmpNumOrd = comparing fst

highestVal :: [Card] -> Int
highestVal = fst . maximumBy cmpNumOrd

isSequential :: [Int] -> Bool
isSequential [] = True
isSequential xs = let minNum = minimum xs
                      indexedNums = zip [0..] $ sort xs
                  in all (\x -> snd x - fst x == minNum) indexedNums

removeFirstNBy :: (a -> Bool) -> Int -> [a] -> [a]
removeFirstNBy _ 0 xs = xs
removeFirstNBy f n xs = let (validYs, invalidYs) = partition f xs
                            remainingYs = drop n validYs
                        in remainingYs ++ invalidYs

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

instance Show Hand where
  show (Hand hc op tp tk s f fh fk sf rf) =
    "\n" ++
    "royal flush    = " ++ show rf ++ "\n" ++
    "straight flush = " ++ show sf ++ "\n" ++
    "four kind      = " ++ show fk ++ "\n" ++
    "full house     = " ++ show fh ++ "\n" ++
    "flush          = " ++ show f ++ "\n" ++
    "straight       = " ++ show s ++ "\n" ++
    "three kind     = " ++ show tk ++ "\n" ++
    "two pairs      = " ++ show tp ++ "\n" ++
    "one pair       = " ++ show op ++ "\n" ++
    "highest card   = " ++ show hc

-- QUESTION:

-- In the card game poker, a hand consists of five cards and are ranked, from lowest to highest, in the following way:

-- Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.
-- Straight Flush: All cards are consecutive values of same suit.
-- Four of a Kind: Four cards of the same value.
-- Full House: Three of a kind and a pair.
-- Flush: All cards of the same suit.
-- Straight: All cards are consecutive values.
-- Three of a Kind: Three cards of the same value.
-- Two Pairs: Two different pairs.
-- One Pair: Two cards of the same value.
-- High Card: Highest value card.

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
