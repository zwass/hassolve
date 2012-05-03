{-# OPTIONS -Wall #-}

module Solver where


--For storing whose turn it is
data Player = PlayerOne | PlayerTwo
            deriving (Show, Read, Eq)

nextPlayer :: Player -> Player
nextPlayer PlayerOne = PlayerTwo
nextPlayer PlayerTwo = PlayerOne

--For storing the value of a move/state
--Win denotes a win for player one
--Lose denotes a win for player two
data Value = Undecided | Lose | Tie | Win
           deriving (Show, Read, Eq, Ord)

{-instance Monoid Value where
  mempty = Undecided
  mappend Win _ = Win
  mappend _ Win = Win
  mappend Tie _ = Tie
  mappend _ Tie = Tie
  mappend Lose _ = Lose
  mappend _ Lose = Lose
  mappend _ _ = Undecided

foldValues :: [Value] -> Value
foldValues = foldl' mappend Undecided-}

newtype Board a = Board a
                deriving (Show, Read)

getBoard :: Board a -> a
getBoard (Board a) = a

--For storing hashed boards
type HashedBoard = Integer

--For storing hashed moves
type Move = Integer

--Everything we need to know for a game state
data GameState a = GameState {value :: Value,
                              board :: Board a}
                   deriving (Show, Read)
--A game tree
data GameTree a = Node (GameState a) [GameTree a]
                | Leaf (GameState a)
                deriving (Show, Read)

getState :: GameTree a -> GameState a
getState (Node s _) = s
getState (Leaf s) = s

--exploreTree getIP doMove primitive genMoves =
--solveGame :: SolvableGame g => g -> GameTree
--solveGame = generateMoves g . (getInitialPosition g)


--get the initial position of tic tac toe

class SolvableGame a where
  getInitialPosition :: Board a
  doMove :: Board a -> Move -> Board a
  primitive :: Board a -> Value
  generateMoves :: Board a -> [Move]
  whoseTurn :: Board a -> Player

maxValue :: SolvableGame a => [GameTree a] -> Value
maxValue = maximum . map (value . getState)

playerOneMax :: SolvableGame a => [GameTree a] -> Value
playerOneMax = maxValue

playerTwoMax :: SolvableGame a => [GameTree a] -> Value
playerTwoMax = minimum . filter (/= Undecided) . map (value . getState)

solve :: SolvableGame a => GameTree a
solve = exploreTree getInitialPosition

exploreTree :: SolvableGame a => Board a -> GameTree a
exploreTree b
  | primitive b /= Undecided = Leaf $ GameState (primitive b) b
exploreTree b = case generateMoves b of
  [] -> Leaf $ GameState (primitive b) b
  ms ->
    let children = map exploreTree . map (doMove b) $ ms
        getMax = if whoseTurn b == PlayerOne then playerOneMax else playerTwoMax in
    Node (GameState (getMax children) b) children

getGameVal :: SolvableGame a => GameTree a -> Value
getGameVal = value . getState
