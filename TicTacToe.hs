module TicTacToe where

import Board



-- | Using an existing board, the mark to make, and the coordinates of the
-- Cell in which to place the mark, move produces the Board resulting from
-- placing that mark on the given board.
move :: Board -> Cell -> Loc -> Board

move (Board board) player (x,y)
  | head moveCol == Empty
    = Board $ leadRows ++ [leadCols ++ [player] ++ tailCols] ++ tailRows
  | otherwise
    = error "move: location already taken"
  where (leadRows,restRows) = splitAt y board
        (moveRow, tailRows) = splitAt 1 restRows
        (leadCols,restCols) = splitAt x $ head moveRow
        (moveCol, tailCols) = splitAt 1 restCols
