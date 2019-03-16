{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Arrow                  ( (***) )
import           Control.Applicative            ( (<|>) )
import           Data.Char                      ( digitToInt )
import           Data.Monoid                    ( First(..) )
import           Data.Maybe                     ( catMaybes )

import           Helpers_54
import           Typedefs_54

main :: IO ()
main = do input <- readFile "input.txt"
          let nonNormalizedCards = inputToCards input
              cards       = map (map normalizeCard *** map normalizeCard) nonNormalizedCards
              cardsToHand = snd . evalCards
              hands       = map (cardsToHand *** cardsToHand) cards
              winners     = map (uncurry evalHands) hands
          print . length . filter (== Player1) . catMaybes $ winners

-- Input processing
toCardDraw :: [String] -> [NonNormalizedCard]
toCardDraw = map (\s -> (head s, s !! 1))

inputToCards :: String -> [Versus [NonNormalizedCard]]
inputToCards s = let handCardCount = 5
                     cardStrGrps   = chunksOf (handCardCount * 2) . words $ s
                     player1Cards  = map (toCardDraw . take handCardCount) cardStrGrps
                     player2Cards  = map (toCardDraw . drop handCardCount) cardStrGrps
                 in zip player1Cards player2Cards

normalizeCard :: NonNormalizedCard -> Card
normalizeCard ('T', s) = (10, s)
normalizeCard ('J', s) = (11, s)
normalizeCard ('Q', s) = (12, s)
normalizeCard ('K', s) = (13, s)
normalizeCard ('A', s) = (14, s)
normalizeCard (i, s) = (digitToInt i, s)

-- Logic processing
returnp :: [Card] -> PokerHand
returnp cs = (cs, blankHand)

(>>~) :: PokerHand -> ([Card] -> PokerHand) -> PokerHand
(>>~) (cs, h) f =
  let (cs', h') = f cs
      hMerged   = calcNewHand h h'
  in  (cs', hMerged)

calcNewHand :: Hand -> Hand -> Hand
calcNewHand (Hand hc  op  tp  tk  s  f  fh  fk  sf  rf )
            (Hand hc' op' tp' tk' s' f' fh' fk' sf' rf')
            = Hand (if hc > hc' then hc else hc')
                   (op <|> op')
                   (tp <|> tp')
                   (tk <|> tk')
                   (s <|> s')
                   (f <|> f')
                   (fh <|> fh')
                   (fk <|> fk')
                   (sf <|> sf')
                   (rf || rf')

evalCards :: [Card] -> PokerHand
evalCards cs = returnp cs 
                          >>~ royalFlush
                          >>~ straightFlush
                          >>~ fourKind False
                          >>~ fullHouse
                          >>~ flush
                          >>~ straight
                          >>~ threeKind False
                          >>~ twoPairs False
                          >>~ onePair False
                          >>~ highestCard

-- Result post-processing
evalHands :: Hand -> Hand -> Maybe Winner
evalHands (Hand hc  op  tp  tk  s  f  fh  fk  sf  rf )
          (Hand hc' op' tp' tk' s' f' fh' fk' sf' rf')
          = getFirst (
              First (evalBools rf rf') <>
              First (evalMaybeInts sf sf') <>
              First (evalMaybeInts fk fk') <>
              First (evalMaybeIntTups fh fh') <>
              First (evalMaybeInts f f') <>
              First (evalMaybeInts s s') <>
              First (evalMaybeInts tk tk') <>
              First (evalMaybeIntTups tp tp') <>
              First (evalMaybeInts op op') <>
              First (evalInts hc hc')
            )

-- Engines
blankHand :: Hand
blankHand = Hand (-1) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing False

onePair :: ConsumeCards -> [Card] -> PokerHand
onePair rmCs cs = (remainingCs, blankHand { opVal = highestNum })
  where (remainingCs, highestNum) = getGrp rmCs 1 2 cs

twoPairs :: ConsumeCards -> [Card] -> PokerHand
twoPairs rmCs cs = let (fstCs, fstHand) = returnp cs >>~ onePair True
                       (sndCs, sndHand) = returnp fstCs >>~ onePair True

                       remainingCs = if rmCs then sndCs else cs
                       Hand { opVal = fstHandOp } = fstHand
                       Hand { opVal = sndHandOp } = sndHand
                   in (remainingCs, blankHand { tpVal = (,) <$> fstHandOp <*> sndHandOp })

threeKind :: ConsumeCards -> [Card] -> PokerHand
threeKind rmCs cs = (remainingCs, blankHand { tkVal = highestNum })
  where (remainingCs, highestNum) = getGrp rmCs 1 3 cs

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
fullHouse cs = let (cs', Hand { tkVal = tk, opVal = op })
                    = returnp cs >>~ (threeKind True) >>~ (onePair True)
               in (cs, blankHand { fhVal = (,) <$> tk <*> op })

fourKind :: ConsumeCards -> [Card] -> PokerHand
fourKind rmCs cs = (remainingCs, blankHand { fkVal = highestNum })
  where (remainingCs, highestNum) = getGrp rmCs 1 4 cs

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