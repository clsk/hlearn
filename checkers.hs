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

instance a => Piece Maybe a where
	show (Piece c True) = show c ++ "K"
	show (Piece c False) = show c
	show Nothing = "-"

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

pieceStatus :: Maybe Piece -> (Int, Int)
pieceStatus Nothing = (0, 0)
pieceStatus (Just (Piece color _)) 
	| color == Red 		= (1, 0)
	| color == Black 	= (0, 1)


rowStatus :: Row -> (Int, Int)
rowStatus = foldl (\ (r, b) piece -> let status = pieceStatus piece in (fst status + r, snd status + b) ) (0, 0)

boardStatus :: Board -> (Int, Int)
boardStatus = foldl (\ (r, b) row -> let status = rowStatus row in (fst status + r, snd status + b)) (0, 0) 

getPiece :: Point -> Board -> Maybe Piece
getPiece (Point x y) board = (board !! y) !! x

replace :: Int -> a -> [a] -> [a]
replace 0 p (x:xs) = p : xs
replace i p (x:xs) = x : replace (i-1) p xs

setPiece :: Point -> Maybe Piece -> Board -> Board
setPiece (Point x y) piece board = replace y (replace x piece (board !! y)) board

movePiece :: Point -> Point -> Board -> Board
movePiece from@(Point x1 y1) to@(Point x2 y2) board = let fp = getPiece from board
												in setPiece to fp (setPiece from Nothing board)


showBoard :: Board -> String
showBoard board = foldl (\ s row -> s ++ show row ++ "\n") "" board
