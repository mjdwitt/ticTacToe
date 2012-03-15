module Board 
  ( Cell (X, O)
  , Loc
  , Board ()
  , makeBoard
  , getCell
  , getRow
  , getCol
  , getDiag
  , listRows
  , listCols
  , listDiags
  , unsafeRow
  , unsafeCol
  , unsafeDiag
  , sideLength
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
    where build x = unsafeRow board x
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



-- | 'getCell' @board loc@ returns the value of the cell at @loc@ on
-- @board@, wrapped in @Just@. If @loc@ is invalid (i.e. not on the
-- @board@) it returns @Nothing@.
getCell :: Board -> Loc -> Maybe Cell

getCell (Board b) loc = loc `M.lookup` b



-- | The following accessors return lists containing the cells in the
-- specified row, column, or diagonal. Rows and columns start counting
-- from zero and there are only two returnable diagonals: zero and one.
-- The returned list will be the same length as the number provided to
-- makeBoard.

unsafeRow :: Board -> Int -> [Cell]

unsafeRow (Board b) row = cellsOnly . toList $ filterWithKey matchRow b
  where matchRow (_,r) _ = row == r

unsafeCol :: Board -> Int -> [Cell]

unsafeCol (Board b) col = cellsOnly . toList $ filterWithKey matchCol b
  where matchCol (c,_) _ = col == c

unsafeDiag :: Board -> Int -> [Cell]

unsafeDiag bd@(Board b) diag = cellsOnly . toList $
  filterWithKey matchDiag b
  where matchDiag (c,r) _ | diag == 0 = c == r
                          | otherwise = c+r == sideLength bd - 1



-- | Some safe versions of the above. Really just wrapper functions.

getRow :: Board -> Int -> Maybe [Cell]
getRow board row | row `outOfBounds` board = Nothing
                 | otherwise               = Just $ unsafeRow board row

getCol :: Board -> Int -> Maybe [Cell]
getCol board col | col `outOfBounds` board = Nothing
                 | otherwise               = Just $ unsafeCol board col

getDiag :: Board -> Int -> Maybe [Cell]
getDiag board diag | diag < 0 || diag > 1  = Nothing
                   | otherwise             = Just $ unsafeDiag board diag



-- | These accessors use the above to return lists of rows, columns, and
-- diagonals.

listRows :: Board -> [[Cell]]
listRows board = generalList (unsafeRow board) board

listCols :: Board -> [[Cell]]
listCols board = generalList (unsafeCol board) board

listDiags :: Board -> [[Cell]]
listDiags board = unsafeDiag board 0 : (unsafeDiag board 1 : [])



-- | 'sideLength' returns the length of a side of the given @Board@.
sideLength :: Board -> Int

sideLength (Board b) = floor
                     . sqrt
                     $ fromIntegral (size b)



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



-- | 'range' returns a list of whole numbers from zero to the length of the
-- given @Board@.
range board = [0..(sideLength board - 1)]



-- | 'outOfBounds' @n board@ checks to see if the value @n@ fits within the
-- dimensions of the @board@. That is, 'outOfBounds' returns @False@ iff
-- @n < sideLength board@.
outOfBounds :: Int -> Board -> Bool

outOfBounds n board = n >= sideLength board



-- | 'generalList' is a generalization used by all the batch accessors. It
-- uses @get@ to return the 2D list of each group of Cells accessible by
-- @get@.
generalList :: (Int -> [Cell]) -> Board -> [[Cell]]

generalList get board = L.map get $ range board
