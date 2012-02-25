module Board 
  ( Cell (X, O)
  , Loc
  , Board ()
  , makeBoard
  , getRow
  , getCol
  , getDiag
  , move
  ) where

import Data.Map as M

import Data.List as L



data Cell = X
          | O
          | Empty
          | Div
            deriving(Eq)

instance Show Cell where
  show X     = " X "
  show O     = " O "
  show Empty = "   "
  show Div   = "---"



type Loc = (Int,Int)



data Board = Board (Map Loc Cell) 
             deriving(Eq)

instance Show Board where
  show board@(Board b) =  "\n"
                       ++ foldr row "" (L.map build $ range board)
                       ++ "\n"
    where build x = getRow board x
          row cells []  = foldr (cell "|") "" cells
          row cells out =  foldr (cell "|") "" cells
                        ++ "\n"
                        ++ foldr (cell "+") "" (L.map (\_ -> Div) $
                                                  range board)
                        ++ "\n"
                        ++ out
          cell _   c []  = show c
          cell div c out = show c ++ div ++ out



-- | 'makeBoard' @size@ creates a Board filled with empty cells. The Board
-- will be a square with sides as long as @size@.
makeBoard :: Int -> Board

makeBoard size = Board . fromList $
  zip (snd $ mapAccumL f 0 [0..(size^2 - 1)]) (repeat Empty)
  where f acc x = (acc+1, (acc `div` size, acc `mod` size))



-- | The following accessors return lists containing the cells in the
-- specified row, column, or diagonal. Rows and columns start counting
-- from zero and there are only two returnable diagonals: zero and one.
-- The returned list will be the same length as the number provided to
-- makeBoard.

getRow :: Board -> Int -> [Cell]

getRow (Board b) row = cellsOnly . toList $ filterWithKey matchRow b
  where matchRow (_,r) _ = row == r

getCol :: Board -> Int -> [Cell]

getCol (Board b) col = cellsOnly . toList $ filterWithKey matchCol b
  where matchCol (c,_) _ = col == c

getDiag :: Board -> Int -> [Cell]

getDiag (Board b) diag = cellsOnly . toList $ filterWithKey matchDiag b
  where matchDiag (c,r) _ | diag == 0 = c == r
                          | otherwise = c+r == 2



-- | These accessors use the above to return lists of rows, columns, and
-- diagonals.

listRows :: Board -> [[Cell]]

listRows board = generalList (getRow board) board

listCols :: Board -> [[Cell]]

listCols board = generalList (getCol board) board

listDiags :: Board -> [[Cell]]

-- total hack-job
listDiags board = take 2 $ generalList (getDiag board) board



-- | 'move' @player loc@ inserts the @player@'s mark at @loc@.
move :: Board -> Cell -> Loc -> (Maybe Board)

move board@(Board b) player loc@(c,r)
  | c < sideLength board && r < sideLength board =
    Just . Board $ M.insert loc player b
  | otherwise    = Nothing



-- Internal functions

-- | 'cellsOnly' takes a list of (Loc,Cell), such as what would be returned
-- by running toList on a Board, and returns a list containing only the
-- cells from the list.
cellsOnly :: [(Loc,Cell)] -> [Cell]

cellsOnly pairs = L.map (\(_,c) -> c) pairs



-- | 'sideLength' returns the length of a side of the given @Board@.
sideLength :: Board -> Int

sideLength (Board b) = floor . sqrt $ fromIntegral (size b)



-- | 'range' returns a list of whole numbers from zero to the length of the
-- given @Board@.
range board = [0..(sideLength board - 1)]



-- | 'generalList' is a generalization used by all the batch accessors. It
-- uses @get@ to return the 2D list of each group of Cells accessible by
-- @get@.
generalList :: (Int -> [Cell]) -> Board -> [[Cell]]

generalList get board = L.map get $ range board
