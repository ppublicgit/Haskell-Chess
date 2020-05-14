module Main where

import System.IO
import ChessBoard

runGame :: Game -> IO ()
runGame gm = forever $ do


main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStrLn "Haskell Chess"
    putStrLn "What is the name of White Player?"
    player1name <- getLine
    let player1 = updatePlayerName whitePlayer player1name
    putStrLn "What is the name of Black Player?"
    player2name <- getLine
    let player2 = updatePlayerName blackPlayer player2name
    let newGame = updateGamePlayers game player1 player2
    runGame newGame
