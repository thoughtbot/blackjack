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
  return ()

setup :: Game
setup = undefined

checkVictory :: Game -> Bool
checkVictory = undefined

newDeck :: Deck
newDeck = undefined


shuffledDeck :: Deck
shuffledDeck = undefined

deal :: Deck -> Player -> (Player, Deck)
deal = undefined

busted :: Player -> Bool
busted = undefined

handValue :: Player -> Int
handValue = undefined

cardValue :: Card -> Int
cardValue = undefined

doDealerTurn :: Dealer -> Deck -> (Dealer, Deck)
doDealerTurn = undefined

doPlayerTurn :: Player -> Deck -> Action -> (Player, Deck)
doPlayerTurn = undefined
