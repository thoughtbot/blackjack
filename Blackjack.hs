module Blackjack where
import System.Random.Shuffle
-- write types
-- annotated fns with empty bodies
-- start high-level

data Card = Numeric Int | Jack | Queen | King | Ace
  deriving (Show)

type Deck = [Card]
type Hand = [Card]

data Player = Player Hand
  deriving (Show)

type Dealer = Player

data Action = Hit | Stay

type Game = (Deck, Player, Dealer)

main :: IO ()
main = do
  let game = setup
  let victory = checkVictory game
  -- deck <- shuffleM newDeck
  print victory

setup :: Game
setup =
  (shuffledDeck, Player [], Player [])

checkVictory :: Game -> Bool
checkVictory (_, player, dealer)
  | playerValue > 21 = False
  | dealerValue > 21 = True
  | playerValue > dealerValue = True
  | otherwise = False
  where playerValue = handValue player
        dealerValue = handValue dealer

newDeck :: Deck
newDeck = undefined

shuffledDeck :: Deck
shuffledDeck = undefined

deal :: Deck -> Player -> (Player, Deck)
deal = undefined

busted :: Player -> Bool
busted = undefined

handValue :: Player -> Int
handValue (Player hand) =
  foldl (+) 0 $ map cardValue hand

cardValue :: Card -> Int
cardValue Ace = 11
cardValue King = 10
cardValue Queen = 10
cardValue Jack = 10
cardValue (Numeric a) = a

doDealerTurn :: Dealer -> Deck -> (Dealer, Deck)
doDealerTurn = undefined

doPlayerTurn :: Player -> Deck -> Action -> (Player, Deck)
doPlayerTurn = undefined
