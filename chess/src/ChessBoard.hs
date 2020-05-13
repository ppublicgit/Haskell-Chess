module ChessBoard
    ( ChessPiece
    , Row
    , Board
    , Pieces
    , Player
    , Color
    , pieceValue
    , Game
    , printBoard
    , startBoard
    , whitePlayer
    , blackPlayer
    , game
    ) where

import Data.List

data ChessPiece = Pawn | Bishop | Knight | Rook | Queen | King
instance Show ChessPiece where
    show Pawn = "P"
    show Bishop = "B"
    show Knight = "N"
    show Rook = "R"
    show Queen = "Q"
    show King = "K"

type Row = [Maybe Piece]

type Board = [Row]

rowSep = "  " ++ (concat (take 8 (repeat " --"))) ++ "\n"
colSep = "|"

printRow :: RowLoc -> Board -> String
printRow rowIndex board = (show ((+) 1 rowIndex)) ++ " |" ++
    (concat . intersperse colSep $ printMaybePiece <$> board !! rowIndex) ++ "|\n"
--    (foldl (\colIdx row -> (printMaybePiece (board !! rowIndex !! colIdx))) ++ (board !! rowIndex))

printBoard :: Board -> String
printBoard board = "   A  B  C  D  E  F  G  H  \n" ++ rowSep
    ++ (concat . intersperse rowSep $ printRow  <$> [0..7] <*> pure board) ++ rowSep

data Piece = Piece Color ChessPiece
instance Show Piece where
    show (Piece c cp) = printPiece c cp

printPiece :: Color -> ChessPiece -> String
printPiece c cp = show c ++ show cp

printMaybePiece :: Maybe Piece -> String
printMaybePiece (Just (Piece c cp)) = printPiece c cp
printMaybePiece (Nothing) = "  "

type Pieces = [ChessPiece]

data Color = Black | White
instance Show Color where
    show Black = "B"
    show White = "W"

data ColLoc = A | B | C | D | E | F | G | H
    deriving (Enum, Show)

type RowLoc = Int

data Location = Location ColLoc RowLoc

colorPiece :: Color -> ChessPiece -> Piece
colorPiece color_in piece_in = Piece color_in piece_in

location :: ColLoc -> RowLoc -> Location
location col row = Location col row

pieceValue :: ChessPiece -> Int
pieceValue Pawn = 1
pieceValue Bishop = 3
pieceValue Knight = 3
pieceValue Rook = 5
pieceValue Queen = 9
pieceValue King = 10

data Player = Player { name :: String
    , color :: Color
    , captured :: Pieces
    , active :: Pieces
    , score :: Int
    } deriving (Show)

data Game = Game { board :: Board
    , player1 :: Player
    , player2 :: Player
    }

baseRow :: [ChessPiece]
baseRow = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]
pawnRow :: [ChessPiece]
pawnRow = (take 8 (repeat Pawn))
emptyRow :: Row
emptyRow = (take 8 (repeat Nothing))

startBoard :: Board
startBoard = [(Just <$> (colorPiece White) <$> baseRow), (Just <$> (colorPiece White) <$> pawnRow),
    emptyRow, emptyRow, emptyRow, emptyRow, (Just <$> (colorPiece Black) <$> pawnRow), (Just <$> (colorPiece Black) <$> baseRow)]
--startBoard = (fmap (colorPiece White) baseRow) ++ (fmap (colorPiece White) pawnRow) ++
--    (take 4 (repeat emptyRow)) ++ (fmap (colorPiece Black) baseRow) ++ (fmap (colorPiece Black) pawnRow)

whitePlayer = Player { name = "Player 1"
    , color = White
    , captured = []
    , active = (take 8 (repeat Pawn)) ++ [Bishop, Bishop, Knight, Knight, Rook, Rook, Queen, King]
    , score = 0
    }

blackPlayer = Player { name = "Player 2"
    , color = Black
    , captured = []
    , active = (take 8 (repeat Pawn)) ++ [Bishop, Bishop, Knight, Knight, Rook, Rook, Queen, King]
    , score = 0
    }

game = Game { board = startBoard
    , player1 = whitePlayer
    , player2 = blackPlayer
    }
