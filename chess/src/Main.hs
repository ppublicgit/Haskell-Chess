module Main where

--import Control.Monad(forever)
import System.Exit(exitSuccess)
import Data.Char(toUpper, ord)
import System.IO
import Data.List(elem)

import ChessBoard

checkCapturedKing :: Game -> Int -> Bool
checkCapturedKing (Game _ (Player _ _ _ act1 _) (Player _ _ _ act2 _)) playerTurn
    | playerTurn == 1 = not $ elem King act1
    | playerTurn == 2 = not $ elem King act2
    | otherwise = False

congratsString :: Game -> Int -> String
congratsString (Game _ (Player name1 _ _ _ _) (Player name2 _ _ _ _)) playerTurn
    | playerTurn == 1 = "Congrats " <> name2 <> ". You have beaten " <> name1 <> " in your game of chess!"
    | playerTurn == 2 = "Congrats " <> name1 <> ". You have beaten " <> name2 <> " in your game of chess!"
    | otherwise = "Neither player wins. What is happening?"

gameOver :: Game -> Int -> IO ()
gameOver gm int =
    if checkCapturedKing gm int then
      do putStrLn $ congratsString gm int
         exitSuccess
    else return ()

askPlayerMove :: String -> String -> Int -> IO ()
askPlayerMove name1 name2 turn = do
    if turn == 1 then
        putStrLn $ "Your move " <> name1 <> ". You are playing as White."
    else
        putStrLn $ "Your move " <> name2 <> ". You are playing as Black."

checkMoveInput :: String -> Bool
checkMoveInput move
    | ((ord . toUpper $ move !! 1) < 49 ) || ((ord . toUpper $ move !! 1) > 56) = False
    | ((ord . toUpper $ move !! 0) < 65 ) || ((ord . toUpper $ move !! 0) > 72) = False
    | otherwise = True

checkPlayerMove :: String -> Bool
checkPlayerMove inputs
    | length inputs /= 5 = False
    | inputs !! 2 /= ' ' = False
    | otherwise = (checkMoveInput $ take 2 inputs) && (checkMoveInput $ reverse . take 2 $ reverse inputs)

extractSingleMove :: String -> Location
extractSingleMove move = Location col row
    where row = flip (-) 49 $ ord . toUpper $ move !! 1
          col = flip (-) 65 $ ord . toUpper $ move !! 0

extractMove :: String -> (Location, Location)
extractMove inputMove = ((extractSingleMove $ take 2 inputMove), (extractSingleMove $ reverse . take 2 $ reverse inputMove))

getPlayerMove :: Game -> Int -> IO (Location, Location)
getPlayerMove gm@(Game _ (Player name1 _ _ _ _) (Player name2 _ _ _ _)) playerTurn = do
    askPlayerMove name1 name2 playerTurn
    playerMove <- getLine
    if checkPlayerMove playerMove then
        return $ extractMove playerMove
    else do
        putStrLn $ "Invalid move input. Specify moves by two characters of column row for start, and then two characters for column and row as end. Example: A3 C5"
        mv <- getPlayerMove gm playerTurn
        return mv

nextTurn :: Int -> Int
nextTurn 1 = 2
nextTurn _ = 1

makeMove :: Game -> Location -> Location -> Int -> Game
makeMove gm@(Game bd _ _) ls le turn = updateAll gm madeMove turn
    where madeMove = updateBoard bd ls le

updateAll :: Game -> (Board, Maybe Piece) -> Int -> Game
updateAll gm (newBoard, Nothing) _ = updateGameBoard gm newBoard
updateAll gm@(Game _ p1 p2) (newBoard, (Just (Piece _ cp))) 1 =
    updateGameBoard (updateGamePlayers gm newPlayer1 newPlayer2) newBoard
    where newPlayer1 = updateScore (updateCaptured p1 cp) (pieceValue cp)
          newPlayer2 = updateActive p2 cp
updateAll gm@(Game _ p1 p2) (newBoard, (Just (Piece _ cp))) _ =
    updateGameBoard (updateGamePlayers gm newPlayer1 newPlayer2) newBoard
    where newPlayer2 = updateScore (updateCaptured p2 cp) (pieceValue cp)
          newPlayer1 = updateActive p1 cp

runGame :: Game -> Int -> IO ()
runGame gm@(Game board _ _) playerTurn = do
    putStrLn $ printBoard board
    gameOver gm playerTurn
    move <- getPlayerMove gm playerTurn
    if isValidPlayerMove gm (fst move) (snd move) (turnToColor playerTurn) then
        runGame (makeMove gm (fst move) (snd move) playerTurn) (nextTurn playerTurn)
    else do
        putStrLn $ "Invalid move specified."
        runGame gm playerTurn



main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStrLn "Haskell Chess"
    putStrLn "What is the name of Player 1 (White)?"
    player1name <- getLine
    let player1 = updatePlayerName whitePlayer player1name
    putStrLn "What is the name of Player 2 (Black)?"
    player2name <- getLine
    let player2 = updatePlayerName blackPlayer player2name
    let newGame = updateGamePlayers game player1 player2
    putStrLn $ "When it is your turn, specify moves by two characters of column row for start, and then two characters for column and row as end. Example: A3 C5"
    runGame newGame 1
