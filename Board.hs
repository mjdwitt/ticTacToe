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
