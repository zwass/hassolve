{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS -Wall #-}

module OneTwoTen where

import Solver

data OTTBoard = OTTBoard {turn :: Player,
                          ottBoard :: Integer}
                deriving (Show, Read)

boardSize :: Integer
boardSize = 5

instance SolvableGame OTTBoard where
  getInitialPosition = Board $ OTTBoard PlayerOne 0
  doMove b m =
    Board $
    OTTBoard (nextPlayer . turn $ getBoard b) (m + (ottBoard $ getBoard b))
  primitive = ottPrimitive . getBoard
  generateMoves _ = [1, 2]
  whoseTurn = turn . getBoard

ottPrimitive :: OTTBoard -> Value
ottPrimitive b
  | ottBoard b < boardSize = Undecided
  | turn b == PlayerOne = Lose
  | otherwise = Win
