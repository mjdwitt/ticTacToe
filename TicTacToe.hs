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



-- | True if the game has been won, i.e. one player has three marks
-- in a single row, column, or diagonal.
gameWon :: Board -> Bool

gameWon (Board board) = won board
  where won board = rowWon board || colWon board || diagWon board
        rowWon row:rows = snd (foldr homRow (head row, True) row)
                        || rowWon rows
          where homRow cell (c,True) | c == cell = (c,True)
                                     | otherwise = (c,False)
                homRow _    (c,False)            = (c,False)
        rowWon    []    = False
        colWon 
