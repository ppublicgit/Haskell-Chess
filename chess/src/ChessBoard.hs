module ChessBoard where
{--
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
--}

import Data.List
import Data.Maybe (fromJust)

data ChessPiece = Pawn | Bishop | Knight | Rook | Queen | King
    deriving (Eq)
instance Show ChessPiece where
    show Pawn = "P"
    show Bishop = "B"
    show Knight = "N"
    show Rook = "R"
    show Queen = "Q"
    show King = "K"

data Piece = Piece Color ChessPiece
    deriving (Eq)
instance Show Piece where
    show (Piece c cp) = printPiece c cp

printPiece :: Color -> ChessPiece -> String
printPiece c cp = show c ++ show cp

printMaybePiece :: Maybe Piece -> String
printMaybePiece (Just (Piece c cp)) = printPiece c cp
printMaybePiece (Nothing) = "  "

type Pieces = [ChessPiece]

data Color = Black | White deriving (Eq)
instance Show Color where
    show Black = "B"
    show White = "W"

notColor :: Color -> Color
notColor White = Black
notColor Black = White

turnToColor :: Int -> Color
turnToColor 1 = White
turnToColor _ = Black

colorPiece :: Color -> ChessPiece -> Piece
colorPiece color_in piece_in = Piece color_in piece_in

pieceValue :: ChessPiece -> Int
pieceValue Pawn = 1
pieceValue Bishop = 3
pieceValue Knight = 3
pieceValue Rook = 5
pieceValue Queen = 9
pieceValue King = 10

-- Player Functions

data Player = Player { name :: String
    , playerColor :: Color
    , captured :: Pieces
    , active :: Pieces
    , score :: Int
    } deriving (Show)

whitePlayer :: Player
whitePlayer = Player { name = "Player 1"
    , playerColor = White
    , captured = []
    , active = (take 8 (repeat Pawn)) ++ [Bishop, Bishop, Knight, Knight, Rook, Rook, Queen, King]
    , score = 0
    }

blackPlayer :: Player
blackPlayer = Player { name = "Player 2"
    , playerColor = Black
    , captured = []
    , active = (take 8 (repeat Pawn)) ++ [Bishop, Bishop, Knight, Knight, Rook, Rook, Queen, King]
    , score = 0
    }

updatePlayerName :: Player -> String -> Player
updatePlayerName p newname = p { name = newname }

updateCaptured :: Player -> ChessPiece -> Player
updateCaptured p@(Player { captured = cap }) cp = p { captured = cp : cap }

updateActive :: Player -> ChessPiece -> Player
updateActive p@(Player { active = act }) cp = p { active = (removePiece act (elemIndex cp act)) }

removePiece :: [ChessPiece] -> Maybe Int -> [ChessPiece]
removePiece listPieces index
    | index == Nothing = listPieces
    | otherwise =  fst split ++ (tail $ snd split)
    where split = splitAt (fromJust index) listPieces

updateScore :: Player -> Int -> Player
updateScore p@(Player { score = s }) int = p { score = s + int}

printPlayerScore :: Player -> String
printPlayerScore (Player nm cl _ _ sr) =
    "Player " <> nm <> " using " <> show cl <> " has score: " <> show sr

-- Board Functions
type Row = [Maybe Piece]

type Board = [Row]

rowSep :: String
rowSep = "  " ++ (concat . take 8 $ repeat " --") ++ "\n"
colSep :: String
colSep = "|"

printRow :: RowLoc -> Board -> String
printRow rowIndex board = (show $ rowIndex + 1) ++ " |" ++
    (concat . intersperse colSep $ printMaybePiece <$> board !! rowIndex) ++ "|\n"

printBoard :: Board -> String
printBoard board = "   A  B  C  D  E  F  G  H  \n" ++ rowSep
    ++ (concat . intersperse rowSep $ printRow <$> [0..7] <*> pure board) ++ rowSep

updateBoard :: Board -> Location -> Location -> (Board, Maybe Piece)
updateBoard bd ls le@(Location ce re)
    | (bd !! re !! ce) == Nothing = ((movePiece bd ls le), Nothing)
    | otherwise = ((movePiece bd ls le), (bd !! re !! ce))

movePiece :: Board -> Location -> Location -> Board
movePiece bd (Location cs rs) (Location ce re) = (changeBoard (changeBoard bd Nothing cs rs) (bd !! rs !! cs) ce re)

changeBoard :: Board -> Maybe Piece -> ColLoc -> RowLoc -> Board
changeBoard bd cp col row = fst splitRows ++ newRow ++ (tail $ snd splitRows)
    where splitRows = splitAt row bd
          newRow = [fst splitCol ++ cp : (tail $ snd splitCol)]
          splitCol = splitAt col (head $ snd splitRows)

type ColLoc = Int

type RowLoc = Int

data Location = Location ColLoc RowLoc
instance Show Location where
    show (Location col row) = (printCol col) ++ show row

swapLoc :: Location -> Location
swapLoc (Location col row) = (Location row col)

mkDiagLocs :: [Int] -> [Int] -> [Location]
mkDiagLocs [] _ = []
mkDiagLocs _ [] = []
mkDiagLocs (x:xs) (y:ys) = (Location x y) : mkDiagLocs xs ys

printCol :: Int -> String
printCol i
    | i == 0 = "A"
    | i == 1 = "B"
    | i == 2 = "C"
    | i == 3 = "D"
    | i == 4 = "E"
    | i == 5 = "F"
    | i == 6 = "G"
    | i == 7 = "H"
    | otherwise = "Error"

location :: ColLoc -> RowLoc -> Location
location col row = Location col row

baseRow :: [ChessPiece]
baseRow = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]
pawnRow :: [ChessPiece]
pawnRow = (take 8 (repeat Pawn))
emptyRow :: Row
emptyRow = (take 8 (repeat Nothing))

startBoard :: Board
startBoard = [(Just <$> (colorPiece White) <$> baseRow), (Just <$> (colorPiece White) <$> pawnRow),
    emptyRow, emptyRow , emptyRow, emptyRow, (Just <$> (colorPiece Black) <$> pawnRow), (Just <$> (colorPiece Black) <$> baseRow)]

-- Game Functions

data Game = Game { gameBoard :: Board
    , gamePlayer1 :: Player
    , gamePlayer2 :: Player
    }

game :: Game
game = Game { gameBoard = startBoard
    , gamePlayer1 = whitePlayer
    , gamePlayer2 = blackPlayer
    }

updateGamePlayers :: Game -> Player -> Player -> Game
updateGamePlayers g p1 p2 = g { gamePlayer1 = p1, gamePlayer2 = p2 }

updateGameBoard :: Game -> Board -> Game
updateGameBoard g b = g { gameBoard = b }

printScore :: Game -> String
printScore (Game _ p1 p2) = printPlayerScore p1 <> "\n" <> printPlayerScore p2

-- MOVEMENT FUNCTIONS --

isValidPlayerMove :: Game -> Location -> Location -> Color -> Bool
isValidPlayerMove (Game board _ _) locStart@(Location cs rs) locEnd turnColor =
    isValidMoveColor turnColor (board !! rs !! cs) && isValidMove (board !! rs !! cs) locStart locEnd board && (not $ isKingLeftInCheck board locStart locEnd turnColor)

-- King Check checks
isKingLeftInCheck :: Board -> Location -> Location -> Color-> Bool
isKingLeftInCheck board ls le turnColor =
    isKingInCheck newBoard turnColor
    where newBoard = fst $ updateBoard board ls le

isKingInCheck :: Board -> Color -> Bool
isKingInCheck board turn =
    isKingInDiagCheck kingLoc turn board ||
    isKingInStraightCheck kingLoc turn board ||
    isKingInKnightCheck kingLoc turn board ||
    isKingInPawnCheck kingLoc turn board ||
    isKingInKingCheck kingLoc turn board
    where kingLoc = getKingLocation board turn

isKingInPawnCheck :: Maybe Location -> Color -> Board -> Bool
isKingInPawnCheck Nothing _ _ = False
isKingInPawnCheck (Just (Location col row)) color board
    | color == White = (checkPiece board (col + 1) (row + 1)) == Just (Piece (notColor color) Pawn) ||
    (checkPiece board (col - 1) (row + 1)) == Just (Piece (notColor color) Pawn)
    | color == Black = (checkPiece board (col + 1) (row - 1)) == Just (Piece (notColor color) Pawn) ||
    (checkPiece board (col - 1) (row - 1)) == Just (Piece (notColor color) Pawn)
    | otherwise = False

isKingInKingCheck :: Maybe Location -> Color -> Board -> Bool
isKingInKingCheck Nothing _ _ = False
isKingInKingCheck (Just (Location col row)) color board =
    (checkPiece board (col) (row + 1)) == Just (Piece (notColor color) King) ||
    (checkPiece board (col) (row - 1)) == Just (Piece (notColor color) King) ||
    (checkPiece board (col + 1) (row)) == Just (Piece (notColor color) King) ||
    (checkPiece board (col + 1) (row)) == Just (Piece (notColor color) King) ||
    (checkPiece board (col + 1) (row + 1)) == Just (Piece (notColor color) King) ||
    (checkPiece board (col - 1) (row + 1)) == Just (Piece (notColor color) King) ||
    (checkPiece board (col + 1) (row - 1)) == Just (Piece (notColor color) King) ||
    (checkPiece board (col - 1) (row - 1)) == Just (Piece (notColor color) King)

isKingInDiagCheck :: Maybe Location -> Color -> Board -> Bool
isKingInDiagCheck Nothing _ _ = False
isKingInDiagCheck (Just (Location col row)) color board =
    (firstPiece Forward col row board) == Just (Piece (notColor color) Rook) ||
    (firstPiece Backward col row board) == Just (Piece (notColor color) Rook) ||
    (firstPiece Leftt col row board) == Just (Piece (notColor color) Rook) ||
    (firstPiece Rightt col row board) == Just (Piece (notColor color) Rook) ||
    (firstPiece Forward col row board) == Just (Piece (notColor color) Queen) ||
    (firstPiece Backward col row board) == Just (Piece (notColor color) Queen) ||
    (firstPiece Leftt col row board) == Just (Piece (notColor color) Queen) ||
    (firstPiece Rightt col row board) == Just (Piece (notColor color) Queen)

isKingInStraightCheck :: Maybe Location -> Color -> Board -> Bool
isKingInStraightCheck Nothing _ _ = False
isKingInStraightCheck (Just (Location col row)) color board =
    (firstPiece Forward col row board) == Just (Piece (notColor color) Rook) ||
    (firstPiece Backward col row board) == Just (Piece (notColor color) Rook) ||
    (firstPiece Leftt col row board) == Just (Piece (notColor color) Rook) ||
    (firstPiece Rightt col row board) == Just (Piece (notColor color) Rook) ||
    (firstPiece Forward col row board) == Just (Piece (notColor color) Queen) ||
    (firstPiece Backward col row board) == Just (Piece (notColor color) Queen) ||
    (firstPiece Leftt col row board) == Just (Piece (notColor color) Queen) ||
    (firstPiece Rightt col row board) == Just (Piece (notColor color) Queen)

data Direction = Forward | Backward | Leftt | Rightt | DiagFR | DiagBR | DiagFL | DiagBL
    deriving (Eq)

firstPiece :: Direction -> ColLoc -> RowLoc -> Board -> Maybe Piece
firstPiece dir col row bd
    | col > 7 || col < 0 || row > 7 || row < 0 = Nothing
    | dir == Forward && (bd !! row !! col == Nothing) = firstPiece dir col (row + 1) bd
    | dir == Backward && (bd !! row !! col == Nothing) = firstPiece dir col (row - 1) bd
    | dir == Rightt && (bd !! row !! col == Nothing) = firstPiece dir (col + 1) row bd
    | dir == Leftt && (bd !! row !! col == Nothing) = firstPiece dir (col - 1) row bd
    | dir == DiagFR && (bd !! row !! col == Nothing) = firstPiece dir (col + 1) (row + 1) bd
    | dir == DiagFL && (bd !! row !! col == Nothing) = firstPiece dir (col - 1) (row + 1) bd
    | dir == DiagBR && (bd !! row !! col == Nothing) = firstPiece dir (col + 1) (row - 1) bd
    | dir == DiagBL && (bd !! row !! col == Nothing) = firstPiece dir (col - 1) (row - 1) bd
    | otherwise = bd !! row !! col


isKingInKnightCheck :: Maybe Location -> Color -> Board -> Bool
isKingInKnightCheck Nothing _ _ = False
isKingInKnightCheck (Just (Location col row)) color board =
    (checkPiece board (col + 2) (row + 1)) == Just (Piece (notColor color) Knight) ||
    (checkPiece board (col + 2) (row - 1)) == Just (Piece (notColor color) Knight) ||
    (checkPiece board (col - 2) (row + 1)) == Just (Piece (notColor color) Knight) ||
    (checkPiece board (col - 2) (row - 1)) == Just (Piece (notColor color) Knight) ||
    (checkPiece board (col + 1) (row + 2)) == Just (Piece (notColor color) Knight) ||
    (checkPiece board (col - 1) (row + 2)) == Just (Piece (notColor color) Knight) ||
    (checkPiece board (col + 1) (row - 2)) == Just (Piece (notColor color) Knight) ||
    (checkPiece board (col - 1) (row - 2)) == Just (Piece (notColor color) Knight)

checkPiece :: Board -> ColLoc -> RowLoc -> Maybe Piece
checkPiece board col row
    | col > 7 || col < 0 || row > 7 || row < 0 = Nothing
    | otherwise = board !! row !! col

getKingLocation :: Board -> Color -> Maybe Location
getKingLocation board color = getKingInBoard board color 0 0

getKingInBoard :: Board -> Color -> ColLoc -> RowLoc -> Maybe Location
getKingInBoard board color col row
    | col == 8 = getKingInBoard board color 0 (row + 1)
    | board !! row !! col == Just (Piece color King) = Just (Location col row)
    | col < 8 = getKingInBoard board color (col + 1) row
    | otherwise = Nothing

-- Valid move check

isValidMoveColor :: Color -> Maybe Piece -> Bool
isValidMoveColor _ Nothing = False
isValidMoveColor White (Just (Piece White _)) = True
isValidMoveColor Black (Just (Piece Black _)) = True
isValidMoveColor _ _ = False

isValidMove :: Maybe Piece -> Location -> Location -> Board -> Bool
isValidMove Nothing _ _ _ = False
isValidMove (Just (Piece color piece)) locStart locEnd board
    | piece == Pawn = isValidPawnMove (Piece color piece) locStart locEnd board
    | piece == Bishop = isValidBishopMove (Piece color piece) locStart locEnd board
    | piece == Knight = isValidKnightMove (Piece color piece) locStart locEnd board
    | piece == Rook = isValidRookMove (Piece color piece) locStart locEnd board
    | piece == Queen = isValidQueenMove (Piece color piece) locStart locEnd board
    | piece == King = isValidKingMove (Piece color piece) locStart locEnd board
    | otherwise = False

isValidMovementPiece :: Piece -> Location -> Location -> Bool
isValidMovementPiece piece (Location colStart rowStart) (Location colEnd rowEnd)
    | colStart < 0 = False
    | rowStart < 0 = False
    | colStart > 7 = False
    | rowStart > 7 = False
    | colEnd < 0 = False
    | rowEnd < 0 = False
    | colEnd > 7 = False
    | rowEnd > 7 = False
    | otherwise = isValidMovement piece (Location colStart rowStart) (Location colEnd rowEnd)

isValidMovement :: Piece -> Location -> Location -> Bool
isValidMovement (Piece cl cp) locStart locEnd =
    case cp of Pawn -> isValidPawnMovement cl locStart locEnd
               Bishop -> isValidBishopMovement locStart locEnd
               Knight -> isValidKnightMovement locStart locEnd
               Rook -> isValidRookMovement locStart locEnd
               Queen -> isValidQueenMovement locStart locEnd
               King -> isValidKingMovement locStart locEnd

isClearPath :: Location -> Location -> Board -> Bool
isClearPath (Location cs rs) (Location ce re) board
    | cs == ce && re == rs = False
    | cs == ce && re > rs = or . init . tail $ isOpen <$> Location cs <$> [rs..re] <*> pure board
    | cs == ce && re < rs = or . init . tail $ isOpen <$> Location cs <$> [re..rs] <*> pure board
    | rs == re && ce > cs = or . init . tail $ isOpen <$> swapLoc <$> Location rs <$> [cs..ce] <*> pure board
    | rs == re && ce < cs = or . init . tail $ isOpen <$> swapLoc <$> Location rs <$> [ce..cs] <*> pure board
    | re > rs && ce > cs = or . init . tail $ isOpen <$> (mkDiagLocs [cs..ce] [rs..re]) <*> pure board
    | re < rs && ce > cs = or . init . tail $ isOpen <$> (mkDiagLocs [cs..ce] (reverse [re..rs])) <*> pure board
    | re > rs && ce < cs = or . init . tail $ isOpen <$> (mkDiagLocs (reverse [cs..ce]) [rs..re]) <*> pure board
    | re < rs && ce < cs = or . init . tail $ isOpen <$> (mkDiagLocs [ce..cs] [re..rs]) <*> pure board
    | otherwise = False

isOpen :: Location -> Board -> Bool
isOpen (Location col row) board = (board !! row !! col) == Nothing

isEnemyPiece :: Color -> Maybe Piece -> Bool
isEnemyPiece _ Nothing = False
isEnemyPiece color (Just (Piece enemyColor _)) = color /= enemyColor


isValidPawnMove :: Piece -> Location -> Location -> Board -> Bool
isValidPawnMove (Piece color _) locStart locEnd board =
    (isValidPawnAttack color locStart locEnd board) || ((isValidPawnMovement color locStart locEnd) && (isOpen locEnd board) && (isClearPath locStart locEnd board))

isValidPawnAttack :: Color -> Location -> Location -> Board -> Bool
isValidPawnAttack color (Location cs rs) (Location ce re) board
    | (abs (cs - ce)) /= 1 = False
    | color == Black && rs - re /= 1 = False
    | color == White && re - rs /= 1 = False
    | (board !! re !! ce) /= Nothing = isEnemyPiece color (board !! re !! ce)
    | otherwise = False

isValidPawnMovement :: Color -> Location -> Location -> Bool
isValidPawnMovement color (Location cs rs) (Location ce re)
    | cs /= ce = False
    | color == Black && rs == 6 && re == 4 = True
    | color == White && rs == 1 && re == 3 = True
    | color == Black && re + 1 /= rs = False
    | color == White && re - 1 /= rs = False
    | otherwise = True

isValidBishopMove :: Piece -> Location -> Location -> Board -> Bool
isValidBishopMove (Piece color _) locStart locEnd@(Location ce re) board =
    (isValidBishopMovement locStart locEnd) && (isClearPath locStart locEnd board) && ((isOpen locEnd board) || (isEnemyPiece color (board !! re !! ce)))

isValidBishopMovement :: Location -> Location -> Bool
isValidBishopMovement (Location cs rs) (Location ce re)
    | (abs (ce - cs)) /= (abs (re - rs)) = False
    | otherwise = True

isValidKnightMove :: Piece -> Location -> Location -> Board -> Bool
isValidKnightMove (Piece color _) locStart locEnd@(Location ce re) board =
    (isValidKnightMovement locStart locEnd) && ((isOpen locEnd board) || (isEnemyPiece color (board !! re !! ce)))

isValidKnightMovement :: Location -> Location -> Bool
isValidKnightMovement (Location cs rs) (Location ce re)
    | (abs (re - rs)) == 1 && (abs (ce - cs)) /= 2 = False
    | (abs (re - rs)) == 2 && (abs (ce - cs)) /= 1 = False
    | otherwise = True

isValidRookMove :: Piece -> Location -> Location -> Board -> Bool
isValidRookMove (Piece color _) locStart locEnd@(Location ce re) board =
    (isValidRookMovement locStart locEnd) && (isClearPath locStart locEnd board) && ((isOpen locEnd board) || (isEnemyPiece color (board !! re !! ce)))

isValidRookMovement :: Location -> Location -> Bool
isValidRookMovement (Location cs rs) (Location ce re)
    | re - rs /= 0 && ce - cs /= 0 = False
    | otherwise = True

isValidQueenMove :: Piece -> Location -> Location -> Board -> Bool
isValidQueenMove (Piece color _) locStart locEnd@(Location ce re) board =
    (isValidQueenMovement locStart locEnd) && (isClearPath locStart locEnd board) && ((isOpen locEnd board) || (isEnemyPiece color (board !! re !! ce)))

isValidQueenMovement :: Location -> Location -> Bool
isValidQueenMovement (Location cs rs) (Location ce re)
    | (abs (ce - cs)) /= (abs (re - rs)) = False
    | re - rs /= 0 && ce - cs /= 0 = False
    | otherwise = True

isValidKingMove :: Piece -> Location -> Location -> Board -> Bool
isValidKingMove (Piece color _) locStart locEnd@(Location ce re) board =
    (isValidKingMovement locStart locEnd) && ((isOpen locEnd board) || (isEnemyPiece color (board !! re !! ce)))

isValidKingMovement :: Location -> Location -> Bool
isValidKingMovement (Location cs rs) (Location ce re)
    | (abs (ce - cs)) > 1 = False
    | (abs (re - rs)) > 1 = False
    | otherwise = True
