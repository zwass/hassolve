module Solver where

import Data.List
import Data.Monoid

--For storing whose turn it is
data Player = PlayerOne | PlayerTwo
            deriving Show

--For storing the value of a move/state
--Win denotes a win for player one
--Lose denotes a win for player two
data Value = Win | Tie | Lose | Undecided
           deriving (Show, Eq)

instance Monoid Value where
  mempty = Undecided
  mappend Win _ = Win
  mappend _ Win = Win
  mappend Tie _ = Tie
  mappend _ Tie = Tie
  mappend Lose _ = Lose
  mappend _ Lose = Lose
  mappend _ _ = Undecided

foldValues :: [Value] -> Value
foldValues = foldl' mappend Undecided

--For storing hashed boards
type HashedBoard = Integer

--For storing hashed moves
type Move = Integer

--Everything we need to know for a game state
data GameState = GameState {turn :: Player,
                            val :: Value,
                            board :: HashedBoard}

--A game tree
data GameTree = Node GameState [GameTree]
              | Leaf GameState
                

--exploreTree getIP doMove primitive genMoves = 
  
  

{-class SolvableGame where
  getInitialPosition :: HashedBoard
  doMove :: HashedBoard -> Move -> HashedBoard
  primitive :: HashedBoard -> Value
  generateMoves :: HashedBoard -> [Move]-}
  