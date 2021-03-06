module Main where

import System.Exit(exitSuccess)
import Data.Char(toUpper, ord)
import System.IO(hSetBuffering, stdout, BufferMode(NoBuffering))
import Data.List(elem)
import Data.Maybe(fromJust)

import ChessBoard

congratsString :: Game -> Int -> String
congratsString (Game _ (Player name1 _ _ _ _) (Player name2 _ _ _ _)) playerTurn
    | playerTurn == 1 = "Congrats " <> name2 <> ". You have beaten " <> name1 <> " in your game of chess!"
    | otherwise = "Congrats " <> name1 <> ". You have beaten " <> name2 <> " in your game of chess!"

stalelmateString :: Game -> String
stalelmateString (Game _ (Player name1 _ _ _ _) (Player name2 _ _ _ _)) = "Boo, you have reached a stalemate. Enjoy your tie " <> name1 <> " and " <> name2 <> "."

gameOver :: Game -> Int -> IO ()
gameOver gm int = do
    let gameFinished = isGameOver gm (turnToColor int)
    if gameFinished == CheckMate then do
        putStrLn $ congratsString gm int
        exitSuccess
    else
        if gameFinished == StaleMate then do
            putStrLn $ stalelmateString gm
            exitSuccess
        else do
            return ()

askPlayerMove :: String -> String -> Int -> IO ()
askPlayerMove name1 name2 turn = do
    if turn == 1 then
        putStrLn $ "Your move " <> name1 <> ". You are playing as White."
    else
        putStrLn $ "Your move " <> name2 <> ". You are playing as Black."

checkMoveInput :: String -> Bool
checkMoveInput move = not . checkMoveInputLimits . makeInputLocation $ (ord . toUpper) <$> [move !! 0, move !! 1]
    where makeInputLocation loc = Location (loc !! 0) (loc !! 1)
          checkMoveInputLimits (Location col row) = col < 65 || col > 72 || row < 49 || row > 56

checkPlayerMove :: String -> Bool
checkPlayerMove inputs
    | length inputs /= 5 || inputs !! 2 /= ' ' = False
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

validPawnPromote :: String -> (Bool, Maybe ChessPiece)
validPawnPromote newPiece
    | newPiece == "Pawn" = (True, Just Pawn)
    | newPiece == "Knight" = (True, Just Knight)
    | newPiece == "Bishop" = (True, Just Bishop)
    | newPiece == "Rook" = (True, Just Rook)
    | newPiece == "Queen" = (True, Just Queen)
    | otherwise = (False, Nothing)

getPawnPromotion :: IO ChessPiece
getPawnPromotion = do
    putStrLn $ "Promoting Pawn. What piece would you like? Pawn, Knight, Bishop, Rook or Queen?"
    input <- getLine
    if fst (validPawnPromote input) then
        return $ fromJust (snd (validPawnPromote input))
    else do
        putStrLn $ "Invalid piece specified for pawn promote."
        ret <- getPawnPromotion
        return ret

runGame :: Game -> Int -> Int -> String -> IO ()
runGame gm@(Game board _ _) playerTurn turnCounter errorString = do
    if turnCounter > 0 then
        putStrLn $ errorString
    else do
        putStrLn $ printBoard board
    gameOver gm playerTurn
    move <- getPlayerMove gm playerTurn
    let isValidResponse = isValidPlayerMove gm (fst move) (snd move) (turnToColor playerTurn)
    if fst isValidResponse then do
        if  checkPawnPromotion gm (fst move) (snd move) then do
            pawnPromotePiece <- getPawnPromotion
            let updatedGame = (promotePawn (makeMove gm (fst move) (snd move) playerTurn) (snd move) pawnPromotePiece)
            gameOver gm playerTurn
            runGame updatedGame (nextTurn playerTurn) 0 ""
        else do
            runGame (makeMove gm (fst move) (snd move) playerTurn) (nextTurn playerTurn) 0 ""
    else do
        runGame gm playerTurn (1 + turnCounter) ("Invalid move specified. " <> (fromJust $ snd isValidResponse))

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
    putStrLn $ "When it is your turn, specify moves by two characters for start column row, and then two characters for end column row. Example: A2 C5"
    runGame newGame 1 0 ""
