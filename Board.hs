module Board where



data Cell = X
          | O
          | Empty
            deriving(Eq)

instance Show Cell where
  show X     = " X "
  show O     = " O "
  show Empty = "   "

data Board = Board [[Cell]]
             deriving(Eq)

instance Show Board where
  show (Board board) = foldr row "" board
    where row :: [Cell] -> String -> String
          row cells []  = foldr cell "" cells
          row cells out = ((foldr cell "" cells)
                          ++ "\n---+---+---\n")
                          ++ out
          cell :: Cell -> String -> String
          cell c []  = show c
          cell c out = ((show c) ++ "|") ++ out

type Loc = (Int,Int)



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
