{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS -Wall #-}

module OneTwoTen where

import Solver

data OTTBoard = OTTBoard {turn :: Player,
                          ottBoard :: Integer}
                deriving (Show, Read)

boardSize :: Integer
boardSize = 10

instance SolvableGame OTTBoard where
  getInitialPosition = OTTBoard PlayerOne 0
  doMove b m =
    OTTBoard (nextPlayer . turn $ b) (m + (ottBoard b))
  primitive = ottPrimitive
  generateMoves _ = [1, 2]
  whoseTurn = turn

ottPrimitive :: OTTBoard -> Value
ottPrimitive b
  | ottBoard b < boardSize = Undecided
  | turn b == PlayerOne = Lose
  | otherwise = Win
