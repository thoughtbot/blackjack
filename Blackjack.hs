import System.Random.Shuffle

data Card = Numeric Int | Jack | Queen | King | Ace
  deriving (Show)

type Deck = [Card]
type Hand = [Card]

data Player = Player Hand
  deriving (Show)

type Dealer = Player

data Action = Hit | Stay
  deriving (Eq)

type Game = (Deck, Player, Dealer)

main :: IO ()
main = do
  deck <- shuffleM newDeck
  let game = setup deck
  game' <- playHand game
  printGame game'
  declareWinner game'

declareWinner :: Game -> IO ()
declareWinner game = do
  let victory = playerVictory game
  if victory
    then putStrLn "You win!"
    else putStrLn "You lose :("

printGame :: Game -> IO ()
printGame (_, player, dealer) = do
  printHand "Dealer's hand: " dealer
  printHand "Your hand: " player

playHand :: Game -> IO Game
playHand (deck, player, dealer) = do
  printHand "Your hand: " player
  printUpCard dealer
  action <- getLine
  let playerAction = makeAction action
  let (player', deck') = doPlayerTurn player deck playerAction
  if playerAction == Hit
      then playHand (deck', player', dealer)
      else return (deck', player', dealer)

printUpCard :: Dealer -> IO ()
printUpCard (Player hand) =
  putStrLn $ "Dealer's card: " ++ (show $ (head hand))

printHand :: [Char] -> Player -> IO ()
printHand message player = do
  putStrLn $ message ++ (show $ handValue player)

makeAction:: [Char] -> Action
makeAction "hit" = Hit
makeAction "stay" = Stay

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
  concat $ replicate 4 $ map Numeric [1..10] ++ [Jack, Queen, King, Ace]

shuffledDeck :: IO Deck
shuffledDeck = shuffleM newDeck

handValue :: Player -> Int
handValue (Player hand) =
-- sumBy ?
  sum $ map cardValue hand

cardValue :: Card -> Int
cardValue Ace = 11
cardValue King = 10
cardValue Queen = 10
cardValue Jack = 10
cardValue (Numeric a) = a

-- doDealerTurn :: Dealer -> Deck -> (Dealer, Deck)
-- doDealerTurn = undefined
--
doPlayerTurn :: Player -> Deck -> Action -> (Player, Deck)
doPlayerTurn (Player hand) deck Hit =
  (Player $ hand ++ take 1 deck, drop 1 deck)
doPlayerTurn player deck Stay = (player, deck)
