module TicTacToe 
  ( Player(X, O)
  , Loc
  , Board()
  , EndGame(..)
  , makeBoard
  , getPlayer
  , getRow
  , getCol
  , getDiag
  , listRows
  , listCols
  , listDiags
  , unsafeRow
  , unsafeCol
  , unsafeDiag
  , move
  , winner
  ) where

import Data.Map as M (Map, lookup, size, fromList, toList,
                      filterWithKey, insert)
import Data.List as L (map, mapAccumL)



------------------------------------------
-- Types and instances
------------------------------------------



data Player = X
            | O
            | Empty
            | Div
              deriving(Eq)

instance Show Player where
  show X     = "X"
  show O     = "O"
  show Empty = " "
  show Div   = "-"



type Loc = (Int,Int)



data Board = Board (Map Loc Player) 
             deriving(Eq)

instance Show Board where
  show board@(Board b) =  "\n"
                       ++ foldr row "" (L.map build $ range board)
                       ++ "\n"
    where build x = unsafeRow board x
          row cells []   = " " ++ foldr (cell " | ") "" cells ++ " "
          row cells out  = " " ++ foldr (cell " | ") "" cells ++ " "
                        ++ "\n-"
                        ++ foldr (cell "-+-") "" (L.map (\_ -> Div) $
                                                  range board)
                        ++ "-\n"
                        ++ out
          cell _   c []  = show c
          cell div c out = show c ++ div ++ out



-- | 'makeBoard' @size@ creates a Board filled with empty cells. The Board
-- will be a square with sides as long as @size@.
makeBoard :: Int -> Board

makeBoard size = Board . fromList $
                 zip [ (x,y) | x <- [0..(size - 1)], y <- [0..(size - 1)] ]
                     (repeat Empty)



-- | Used to control the ending of a game.
data EndGame = Winner Player
             | Draw
             | NotDone
               deriving (Show, Eq)



-- | `winner` @board@ returns @Winner X@ or @Winner O@ if either has three
-- in a row anywhere on the @board@. If all the spaces are filled, then
-- @Draw@ is returned. Otherwise, @NotDone@ is returned.
winner :: Board -> EndGame

winner board | any (all (X ==)) rowsColsAndDiags = Winner X
             | any (all (O ==)) rowsColsAndDiags = Winner O
             | hasEmpties rowsColsAndDiags     = NotDone
             | otherwise                       = Draw
  where rowsColsAndDiags = listRows board
                        ++ listCols board
                        ++ listDiags board
        hasEmpties = any (Empty `elem`)



------------------------------------------
-- Board accessors and modifiers
------------------------------------------



-- | 'move' @player loc@ inserts the @player@'s mark at @loc@.
move :: Board -> Player -> Loc -> (Maybe Board)

move board@(Board b) player loc@(c,r)
  | c < sideLength board
    && r < sideLength board
    && getPlayer board loc == Just Empty
    = Just . Board $ M.insert loc player b
  | otherwise    = Nothing



-- | 'getPlayer' @board loc@ returns the value of the cell at @loc@ on
-- @board@, wrapped in @Just@. If @loc@ is invalid (i.e. not on the
-- @board@) it returns @Nothing@.
getPlayer :: Board -> Loc -> Maybe Player

getPlayer (Board b) loc = loc `M.lookup` b



-- | Some safe accessors which return an entire row, column, or diagonal
-- as a list.

getRow :: Board -> Int -> Maybe [Player]
getRow board row | row `outOfBounds` board = Nothing
                 | otherwise               = Just $ unsafeRow board row

getCol :: Board -> Int -> Maybe [Player]
getCol board col | col `outOfBounds` board = Nothing
                 | otherwise               = Just $ unsafeCol board col

getDiag :: Board -> Int -> Maybe [Player]
getDiag board diag | diag < 0 || diag > 1  = Nothing
                   | otherwise             = Just $ unsafeDiag board diag



-- | These accessors use the unsafe accessors to return lists of rows,
-- columns, and diagonals.

listRows :: Board -> [[Player]]
listRows board = generalList (unsafeRow board) board

listCols :: Board -> [[Player]]
listCols board = generalList (unsafeCol board) board

listDiags :: Board -> [[Player]]
listDiags board = unsafeDiag board 0 : (unsafeDiag board 1 : [])



-- | The following accessors return lists containing the cells in the
-- specified row, column, or diagonal. Rows and columns start counting
-- from zero and there are only two returnable diagonals: zero and one.
-- The returned list will be the same length as the number provided to
-- makeBoard.

unsafeRow :: Board -> Int -> [Player]
unsafeRow (Board b) row = cellsOnly . toList $ filterWithKey matchRow b
  where matchRow (_,r) _ = row == r

unsafeCol :: Board -> Int -> [Player]
unsafeCol (Board b) col = cellsOnly . toList $ filterWithKey matchCol b
  where matchCol (c,_) _ = col == c

unsafeDiag :: Board -> Int -> [Player]
unsafeDiag bd@(Board b) diag = cellsOnly . toList $
  filterWithKey matchDiag b
  where matchDiag (c,r) _ | diag == 0 = c == r
                          | otherwise = c+r == sideLength bd - 1



------------------------------------------
-- Internal functions
------------------------------------------



-- | 'sideLength' returns the length of a side of the given @Board@.
sideLength :: Board -> Int

sideLength (Board b) = floor
                     . sqrt
                     $ fromIntegral (size b)



-- | 'cellsOnly' takes a list of (Loc,Player), such as what would be
-- returned by running toList on a Board, and returns a list containing only
-- the cells from the list.
cellsOnly :: [(Loc,Player)] -> [Player]

cellsOnly pairs = L.map (\(_,c) -> c) pairs



-- | 'range' returns a list of whole numbers from zero to the length of the
-- given @Board@.
range board = [0..(sideLength board - 1)]



-- | 'outOfBounds' @n board@ checks to see if the value @n@ fits within the
-- dimensions of the @board@. That is, 'outOfBounds' returns @False@ iff
-- @n < sideLength board@.
outOfBounds :: Int -> Board -> Bool

outOfBounds n board = n >= sideLength board



-- | 'generalList' is a generalization used by all the batch accessors. It
-- uses @get@ to return the 2D list of each group of Players accessible by
-- @get@.
generalList :: (Int -> [Player]) -> Board -> [[Player]]

generalList get board = L.map get $ range board
