module CLI where



import TicTacToe



main :: IO()
main = do
       putStrLn "Get ready for some ASCii TicTacToe!"
       putStrLn ""
       putStrLn "How many rows and columns should this board have?"
       size <- getLine
       play X . makeBoard $ read size



play :: Player -> Board -> IO()
play p b = do
           putStr $ show b
           case end of
             NotDone   -> do
                          putStr (show p ++ "'s turn. ")
                          turn p b
             otherwise -> gameOver end
  where end = winner b



turn :: Player -> Board -> IO()
turn p b = do
           putStrLn "Where would you like to go?"
           putStrLn "Select a column: "
           col <- getLine
           putStrLn "Select a row: "
           row <- getLine
           case move b p (read col, read row) of
             Just new -> play (nextPlayer p) new
             Nothing  -> do
                         putStrLn "That was an invalid move. Try again."
                         turn p b



gameOver :: EndGame -> IO()
gameOver end = do
               putStr "Game over. "
               case end of
                 Winner a -> putStrLn (show a ++ " won!")
                 Draw     -> putStrLn "CAT."



nextPlayer :: Player -> Player
nextPlayer X = O
nextPlayer O = X
nextPlayer _ = error "nextPlayer: only works for X and O"
