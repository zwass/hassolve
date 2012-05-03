{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS -Wall #-}

module TicTacToe where

import Data.List
import Data.List.Split

import Solver

data Piece = Empty | X | O
           deriving (Show, Eq)

type TTTBoard = [Piece]
--2D should hopefully make thinking about primitive easier
type Board2D = [[Piece]]

-- ttt = Game getInitialPosition
--       (generateMoves . unhashBoard)
--       (hashBoard . doMove $ unhashBoard)
--       primitive

--Store moves as the index of the move counting left to right, top to
--bottom

boardSize :: Int
boardSize = 2

-- hashBoard :: TTTBoard -> HashedBoard
-- hashBoard b = foldl' (\h p -> h * 10 + (hashPiece p)) 0 b

-- hashPiece :: Piece -> Integer
-- hashPiece Empty = 0
-- hashPiece X = 1
-- hashPiece O = 2

-- --Can we use unfoldr?
-- unhashBoard :: HashedBoard -> TTTBoard
-- unhashBoard b = reverse $ unhashBoardHelper (boardSize ^ 2) b

-- unhashBoardHelper :: Int -> HashedBoard -> [Piece]
-- unhashBoardHelper 0 _ = []
-- unhashBoardHelper s b = (unhashPiece (b `mod` 10)) :
--                         unhashBoardHelper (s - 1)  (b `div` 10)

-- unhashPiece :: Integer -> Piece
-- unhashPiece 0 = Empty
-- unhashPiece 1 = X
-- unhashPiece 2 = O

tttGetInitialPosition :: TTTBoard
tttGetInitialPosition = take (boardSize ^ 2) $ repeat Empty

--Returns a count of each type of piece, Xs first
pieceCounts :: TTTBoard -> (Int, Int)
pieceCounts = foldl'
              (\(x, o) p -> case p of X -> (x + 1, o)
                                      O -> (x, o + 1)
                                      Empty -> (x, o))
              (0, 0)

tttWhoseTurn :: TTTBoard -> Player
tttWhoseTurn b
  | (fst counts) == (snd counts) = PlayerOne
  | otherwise = PlayerTwo
                where counts = pieceCounts b

--Params are index, piece type, original board
placePiece :: Move -> Piece -> TTTBoard -> TTTBoard
placePiece i t = map (\(li, p) -> if li == i then t else p) . zip [1..]

tttDoMove :: TTTBoard -> Move -> TTTBoard
tttDoMove b m = case (tttWhoseTurn b) of
  PlayerOne -> placePiece m X b
  PlayerTwo -> placePiece m O b

boardTo2D :: TTTBoard -> Board2D
boardTo2D = chunk boardSize

--get all possible indices on the 2D board. This useful?
indexPerms :: [(Int, Int)]
indexPerms = concat $
             map (\x -> zip (repeat x) [1..boardSize]) [1..boardSize]

reflect :: [[a]] -> [[a]]
reflect = map reverse

pieceAt :: Board2D -> (Int, Int) -> Piece
pieceAt b (row, col) = b !! (row-1) !! (col-1)

--Generate rows/diags and pass them to this function to check for 3
--in a row-ness
checkValue :: [Piece] -> Value
checkValue p = case (filter (\l -> length l >= 3) (group p)) of
  [] -> Undecided
  [X:_] -> Win
  otherwise -> Lose

getRows :: Board2D -> [[Piece]]
getRows = id

getDiag :: Board2D -> (Int, Int) -> [Piece]
getDiag b (row, col)
  | row > boardSize || col > boardSize = []
  | otherwise = pieceAt b (row, col) : getDiag b (row+1, col+1)

getDiags :: Board2D -> [[Piece]]
getDiags b = map (getDiag b) $ zip [1..boardSize] (repeat 1)
getAllPossibilities :: Board2D -> [[Piece]]
getAllPossibilities b = getDiags b ++ getRows b ++ getDiags bt ++
                        getRows bt ++ getDiags br
                        where bt = transpose b
                              br = reflect b

checkTie :: TTTBoard -> Value
checkTie b = case (Empty `elem` b) of
  False -> Tie
  True -> Undecided

--Figure out whether someone won or lost
--This is going to be difficult...
--Can we only check in one direction, then transpose
--Like horizontal, and diagonal
--After the transpose we should be getting vertical and the other diagonal
tttPrimitive :: TTTBoard -> Value
tttPrimitive b = maximum $
              checkTie b : (map checkValue $ getAllPossibilities $ b2d)
              where b2d = boardTo2D b

tttGenerateMoves :: TTTBoard -> [Move]
tttGenerateMoves b = map fst $ filter (\(_, p) -> p == Empty) $
                  zip [1..] b

instance SolvableGame TTTBoard where
  getInitialPosition = Board tttGetInitialPosition
  doMove b m = Board (tttDoMove (getBoard b) m)
  primitive = tttPrimitive . getBoard
  generateMoves = tttGenerateMoves . getBoard
  whoseTurn = tttWhoseTurn . getBoard