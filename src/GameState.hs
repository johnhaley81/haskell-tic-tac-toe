module GameState
( GameBoardTile
, GameState(..)
, getGameWinner
, getInitialGameState
, getNextTurn
, isCatsGame
, isPlayerTurn
, isValidMove
, playComputerTurn
, Marker(..)
, numberToTilePosition
, updateBoardForMove
, updateCurrentTurn
) where

import           Data.Maybe (fromJust, isJust, isNothing)
import           Utils      ((|>))

data Marker = X | O deriving (Eq, Show)
data ColPosition = Left | Center | Right deriving (Bounded, Enum, Eq, Ord, Show)
data RowPosition = Top | Middle | Bottom deriving (Bounded, Enum, Eq, Ord, Show)

getNextTurn :: Marker -> Marker
getNextTurn X = O
getNextTurn O = X

getNextColPosition :: ColPosition -> ColPosition
getNextColPosition GameState.Right = GameState.Left
getNextColPosition colPosition     = succ colPosition

getNextRowPosition :: RowPosition -> RowPosition
getNextRowPosition Bottom      = Top
getNextRowPosition rowPosition = succ rowPosition

isLastBoardTile :: RowPosition -> ColPosition -> Bool
isLastBoardTile rowPosition colPosition = rowPosition == maxBound && colPosition == maxBound

data GameBoardTile = GameBoardTile
  { row    :: RowPosition
  , col    :: ColPosition
  , marker :: Maybe Marker
  }
instance Show GameBoardTile where
  show GameBoardTile{ marker = Just X }  = show X
  show GameBoardTile{ marker = Just O }  = show O
  show GameBoardTile{ row = x, col = y } = show (tilePositionToNumber x y)
instance Eq GameBoardTile where
  (==) GameBoardTile{ marker=marker1 } GameBoardTile{ marker=marker2 } = marker1 == marker2 && marker1 |> isJust
instance Ord GameBoardTile where
  compare (GameBoardTile row1 col1 _) (GameBoardTile row2 col2 _) = if row1 == row2 then compare col1 col2 else compare row1 row2

tilePositionToNumber :: RowPosition -> ColPosition -> Int
tilePositionToNumber rowPosition colPosition = ((3 * fromEnum rowPosition) + fromEnum colPosition) + 1 -- + 1 so we don't get overlap with O and 0

numberToTilePosition :: Int -> (RowPosition, ColPosition)
numberToTilePosition num = let
  rowPosition = ((num - 1) `quot` 3) |> toEnum
  colPosition = ((num - 1) `mod` 3) |> toEnum
  in (rowPosition, colPosition)

data GameBoardRow = GameBoardRow
  { leftCol   :: GameBoardTile
  , centerCol :: GameBoardTile
  , rightCol  :: GameBoardTile
  }
instance Show GameBoardRow where
  show GameBoardRow{ leftCol=left, centerCol=center, rightCol=right } = show left ++ "|" ++ show center ++ "|" ++ show right

colSeparators :: String
colSeparators = "\n-----\n"

data GameBoard = GameBoard
  { topRow    :: GameBoardRow
  , middleRow :: GameBoardRow
  , bottomRow :: GameBoardRow
  }
instance Show GameBoard where
  show GameBoard{ topRow=top, middleRow=middle, bottomRow=bottom }
    = "\n" ++ show top ++ colSeparators ++ show middle ++ colSeparators ++ show bottom ++ "\n"

data GameState =
  GameState { playerMarker :: Marker
            , gameBoard    :: GameBoard
            , currentTurn  :: Marker
            }
instance Show GameState where
  show gameState = gameState |> gameBoard |> show

isPlayerTurn :: GameState -> Bool
isPlayerTurn gameState = gameState |> playerMarker == gameState |> currentTurn

-- Super dumb right now, just picks the next spot to play
playComputerTurn :: GameState -> GameState
playComputerTurn gameState
  | gameState |> currentTurn == gameState |> playerMarker = gameState
  | nextBlankTile |> isJust
    = gameState
    { gameBoard = gameState
      |> gameBoard
      |> updateBoardForMarker (nextBlankTile |> fromJust |> fst) (nextBlankTile |> fromJust |> snd)
    }
    |> updateCurrentTurn
  | otherwise = gameState
  where
    updateBoardForMarker = gameState |> currentTurn |> updateBoardForMove
    nextBlankTile = getNextBlankTile (gameState |> gameBoard) (Top, GameState.Left)

updateCurrentTurn :: GameState -> GameState
updateCurrentTurn gameState = gameState { currentTurn = currentTurn gameState |> getNextTurn }



-- Initialization methods and consts
initialGameBoard :: GameBoard
initialGameBoard = GameBoard (getInitialGameRow Top) (getInitialGameRow Middle) (getInitialGameRow Bottom)

getInitialGameBoardTile :: RowPosition -> ColPosition -> GameBoardTile
getInitialGameBoardTile blankRow blankCol = GameBoardTile blankRow blankCol Nothing

getInitialGameRow :: RowPosition -> GameBoardRow
getInitialGameRow rowPosition = let
  getInitialGameBoardTileForRow = getInitialGameBoardTile rowPosition
  in GameBoardRow (getInitialGameBoardTileForRow GameState.Left) (getInitialGameBoardTileForRow Center) (getInitialGameBoardTileForRow GameState.Right)

getInitialGameState :: Marker -> GameState
getInitialGameState newPlayerMarker = GameState newPlayerMarker initialGameBoard X



-- Game board getters
isValidMove :: RowPosition -> ColPosition -> GameBoard -> Bool
isValidMove rowPosition colPosition board = getRowColMarker rowPosition colPosition board |> isNothing

getRowColMarker :: RowPosition -> ColPosition -> GameBoard -> Maybe Marker
getRowColMarker rowPosition colPosition board
  | rowPosition == Top = board |> topRow |> getRowMarker
  | rowPosition == Middle = board |> middleRow |> getRowMarker
  | rowPosition == Bottom = board |> bottomRow |> getRowMarker
  | otherwise = Nothing
  where getRowMarker = getColMarker colPosition

getColMarker :: ColPosition -> GameBoardRow -> Maybe Marker
getColMarker colPosition GameBoardRow{ leftCol=left, centerCol=center, rightCol=right }
  | colPosition == GameState.Left = marker left
  | colPosition == Center = marker center
  | colPosition == GameState.Right = marker right
  | otherwise = Nothing

getNextTile :: (RowPosition, ColPosition) -> (RowPosition, ColPosition)
getNextTile (rowPosition, colPosition) = if colPosition == maxBound
  then (rowPosition |> getNextRowPosition, colPosition |> getNextColPosition)
  else (rowPosition, colPosition |> getNextColPosition)

getNextBlankTile :: GameBoard -> (RowPosition, ColPosition) -> Maybe (RowPosition, ColPosition)
getNextBlankTile gameBoardToCheck (rowPosition, colPosition) = let
  isBlank = getRowColMarker rowPosition colPosition gameBoardToCheck |> isNothing
  in if isBlank
    then Just (rowPosition, colPosition)
    else if isLastBoardTile rowPosition colPosition
      then Nothing
      else getNextBlankTile gameBoardToCheck (getNextTile (rowPosition, colPosition))



-- Board update methods
updateBoardForMove :: Marker -> RowPosition -> ColPosition -> GameBoard -> GameBoard
updateBoardForMove newMarker rowPosition colPosition board
  | rowPosition == Top = board { topRow=board |> topRow |> updateRowForBoard }
  | rowPosition == Middle = board { middleRow=board |> middleRow |> updateRowForBoard }
  | rowPosition == Bottom = board { bottomRow=board |> bottomRow |> updateRowForBoard }
  | otherwise = board
  where
    updateRowForBoard = updateGameBoardRow newMarker colPosition

updateGameBoardRow :: Marker -> ColPosition -> GameBoardRow -> GameBoardRow
updateGameBoardRow newMarker colPosition boardRow
  | colPosition == GameState.Left = boardRow { leftCol=boardRow |> leftCol |> updateGameBoardTile newMarker }
  | colPosition == Center = boardRow { centerCol=boardRow |> centerCol |> updateGameBoardTile newMarker }
  | colPosition == GameState.Right = boardRow { rightCol=boardRow |> rightCol |> updateGameBoardTile newMarker }
  | otherwise = boardRow

updateGameBoardTile :: Marker -> GameBoardTile -> GameBoardTile
updateGameBoardTile newMarker tile = tile { marker=Just newMarker }



-- Win condition functions
getGameWinner :: GameState -> Maybe Marker
getGameWinner GameState{ gameBoard=board }
  | isJust topRowWinner = topRowWinner
  | isJust middleRowWinner = middleRowWinner
  | isJust bottomRowWinner = bottomRowWinner
  | isJust leftColWinner = leftColWinner
  | isJust centerColWinner = centerColWinner
  | isJust rightColWinner = rightColWinner
  | isJust diagonalWinner = diagonalWinner
  | otherwise = Nothing
  where
    top = topRow board
    middle = middleRow board
    bottom = bottomRow board
    topRowWinner = getRowWinner top
    middleRowWinner = getRowWinner middle
    bottomRowWinner = getRowWinner bottom
    getColWinnerForBoard = getColWinner top middle bottom
    leftColWinner = getColWinnerForBoard GameState.Left
    centerColWinner = getColWinnerForBoard Center
    rightColWinner = getColWinnerForBoard GameState.Right
    diagonalWinner = getDiagonalWinner top middle bottom

getRowWinner :: GameBoardRow -> Maybe Marker
getRowWinner GameBoardRow{ leftCol=left, centerCol=center, rightCol=right }
  = if left == right && left == center
    then marker left
    else Nothing

getColWinner :: GameBoardRow -> GameBoardRow -> GameBoardRow -> ColPosition -> Maybe Marker
getColWinner top middle bottom colPosition = let
  getMarker = getColMarker colPosition
  topMarker = getMarker top
  middleMarker = getMarker middle
  bottomMarker = getMarker bottom
  in if topMarker == middleMarker && topMarker == bottomMarker
    then topMarker
    else Nothing

getDiagonalWinner :: GameBoardRow -> GameBoardRow -> GameBoardRow -> Maybe Marker
getDiagonalWinner
  GameBoardRow{ leftCol=leftTop, rightCol=rightTop }
  GameBoardRow{ centerCol=center }
  GameBoardRow{ leftCol=leftBottom, rightCol=rightBottom }
  = if (leftTop == center && center == rightBottom) || (rightTop == center && center == leftBottom)
    then leftTop |> marker
    else Nothing

-- Cat's game checkers
isCatsGame :: GameBoard -> Bool
isCatsGame GameBoard{ topRow=top, middleRow=middle, bottomRow=bottom }
  = top |> isRowFull && middle |> isRowFull && bottom |> isRowFull

isRowFull :: GameBoardRow -> Bool
isRowFull GameBoardRow{ leftCol=left, centerCol=center, rightCol=right }
  = left |> marker |> isJust && center |> marker |> isJust && right |> marker |> isJust
