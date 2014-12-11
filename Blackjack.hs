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
  deck <- shuffleM newDeck
  let game = setup deck
  let victory = playerVictory game
  print game
  print victory

setup :: Deck -> Game
setup deck =
  (deck'', player, dealer)
  where (player, deck') = dealHand deck
        (dealer, deck'') = dealHand deck'

dealHand :: Deck -> (Player, Deck)
dealHand deck =
  (Player $ take 2 deck, drop 2 deck)

playerVictory :: Game -> Bool
playerVictory (_, player, dealer)
  | playerValue > 21 = False
  | dealerValue > 21 = True
  | playerValue > dealerValue = True
  | otherwise = False
  where playerValue = handValue player
        dealerValue = handValue dealer

newDeck :: Deck
newDeck =
  concat $ replicate 4 [Numeric 1, Numeric 2, Numeric 3, Numeric 4, Numeric 5,
                        Numeric 6, Numeric 7, Numeric 8, Numeric 9, Numeric 10,
                        Jack, Queen, King, Ace]

shuffledDeck :: IO Deck
shuffledDeck = shuffleM newDeck

deal :: Deck -> Player -> (Player, Deck)
deal = undefined

busted :: Player -> Bool
busted = undefined

handValue :: Player -> Int
handValue (Player hand) =
  sum $ map cardValue hand

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
