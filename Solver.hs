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

--For storing hashed boards
type HashedBoard = Integer

--For storing hashed moves
type Move = Integer

--Everything we need to know for a game state
data GameState a = GameState {value :: Value,
                              board :: a}
                   deriving (Show, Read)
--A game tree
data GameTree a = Node (GameState a) [GameTree a]
                | Leaf (GameState a)
                deriving (Show, Read)

getState :: GameTree a -> GameState a
getState (Node s _) = s
getState (Leaf s) = s

class SolvableGame a where
  getInitialPosition :: a
  doMove :: a -> Move -> a
  primitive :: a -> Value
  generateMoves :: a -> [Move]
  whoseTurn :: a -> Player

maxValue :: SolvableGame a => [GameTree a] -> Value
maxValue = maximum . map (value . getState)

playerOneMax :: SolvableGame a => [GameTree a] -> Value
playerOneMax = maxValue

playerTwoMax :: SolvableGame a => [GameTree a] -> Value
playerTwoMax = minimum . filter (/= Undecided) . map (value . getState)

solve :: SolvableGame a => GameTree a
solve = exploreTree getInitialPosition

--This is where the magic happens
exploreTree :: SolvableGame a => a -> GameTree a
exploreTree b -- We're at a leaf if primitive is not undecided
  | primitive b /= Undecided = Leaf $ GameState (primitive b) b
exploreTree b = case generateMoves b of
  [] -> Leaf $ GameState (primitive b) b
  ms -> --Explore the child positions of this position
    let children = map exploreTree . map (doMove b) $ ms
        getMax = if whoseTurn b == PlayerOne then playerOneMax else playerTwoMax in
    Node (GameState (getMax children) b) children

getGameVal :: SolvableGame a => GameTree a -> Value
getGameVal = value . getState
