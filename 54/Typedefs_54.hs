module Typedefs_54 (
    ConsumeCards
  , Hand (..)
  , NonNormalizedCard
  , Card
  , PokerHand
  , Versus
  , Winner (..)
) where

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

type NonNormalizedCard = (Char, Char)
type Card = (Int, Char)
type PokerHand = ([Card], Hand)
type Versus a = (a, a)
type ConsumeCards = Bool

data Winner = Player1 | Player2

instance Show Winner where
  show Player1 = "Player 1"
  show Player2 = "Player 2"

instance Eq Winner where
  Player1 == Player1 = True
  Player2 == Player2 = True
  _ == _ = False
