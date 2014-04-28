import Data.List

data Color = Red | Black deriving (Eq)
instance Show Color where
	show Red = "R"
	show Black = "B"
type IsKing = Bool
data Piece = Piece Color IsKing deriving (Eq)
instance Show Piece where
	show (Piece c True) = show c ++ "K"
	show (Piece c False) = show c
data Point = Point Int Int
type Row = [Maybe Piece]
type Board = [Row]

basePiece :: Color -> Maybe Piece
basePiece color = Just $ Piece color False

baseRow :: Int -> Color -> Bool -> [Maybe Piece]
baseRow n c p
	| n < 1 	= []
	| p 		= basePiece c : baseRow (n-1) c (not p)
	| otherwise = Nothing : baseRow (n-1) c (not p)

emptyRow :: [Maybe Piece]
emptyRow = replicate 8 Nothing

baseBoard :: Board
baseBoard = [ 
				baseRow 8 Black True,
				baseRow 8 Black False,
				baseRow 8 Black True,
				replicate 8 Nothing,
				replicate 8 Nothing,
				baseRow 8 Red False,
				baseRow 8 Red True,
				baseRow 8 Red False
			]


getRowPiece :: Point -> Row -> Maybe Piece
getRowPiece (Point x _) row =  row !! x

getRow :: Point -> Board -> Row
getRow (Point _ y) board = board !! y

getPiece :: Point -> Board -> Maybe Piece
getPiece pt board = getRowPiece pt $ getRow pt board

movePiece :: Point -> Point -> Board
movePiece (Point x1 y1) (Point x2 y2) = baseBoard
