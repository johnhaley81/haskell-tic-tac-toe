module Main where

import           Data.Char  (digitToInt)
import           Data.Maybe (fromJust, isJust)
import           GameState
import           Utils      (clearScreen, (|>))

main :: IO ()
main = do
  putStrLn "Let's play Tic-Tac-Toe!"
  putStrLn "Which marker would you like (X/O)?"
  marker <- getMarker
  marker |> getInitialGameState |> playNextTurn

getMarker :: IO Marker
getMarker = do
  markerInput <- getChar
  _ <- getLine
  let
    maybeMarker = charToMarker markerInput
    in if maybeMarker |> isJust
      then maybeMarker |> fromJust |> return
      else do
        putStrLn "Invalid input. Valid inputs are: 'x', 'X', 'o', 'O'"
        getMarker

charToMarker :: Char -> Maybe Marker
charToMarker c
  | isX = Just X
  | isO = Just O
  | otherwise = Nothing
  where isX = c `elem` ['x', 'X']
        isO = c `elem` ['o', 'O']

playNextTurn :: GameState -> IO ()
playNextTurn gameState = do
  let winner = gameState |> getGameWinner
  let catsGame = gameState |> gameBoard |> isCatsGame
  if winner |> isJust || catsGame
    then winner |> fromJust |> printGameResult catsGame
    else if gameState |> isPlayerTurn
      then do
        nextGameState <- gameState |> playPlayerTurn
        nextGameState |> playNextTurn
      else gameState |> playComputerTurn |> playNextTurn

printGameResult :: Bool -> Marker -> IO ()
printGameResult catsGame marker = if catsGame
  then putStrLn "Cat's game!"
  else putStrLn ("The winner is: " ++ show marker ++ "!")

playPlayerTurn :: GameState -> IO GameState
playPlayerTurn gameState = do
  clearScreen
  putStr (gameState |> gameBoard |> show)
  putStrLn "Chose a spot to place your marker: "
  tileNumberChar <- getChar
  if tileNumberChar <= '0' || tileNumberChar > '9'
    then do
      putStrLn "Please enter a number 1-9"
      playPlayerTurn gameState
    else let
      tileNumber = tileNumberChar |> digitToInt
      (rowPosition, colPosition) = tileNumber |> numberToTilePosition
      in if isValidMove rowPosition colPosition (gameState |> gameBoard)
        then return gameState
          { gameBoard = updateBoardForMove (gameState |> playerMarker) rowPosition colPosition (gameState |> gameBoard)
          , currentTurn = gameState |> currentTurn |> getNextTurn
          }
        else do
          putStrLn "Tile already occupied. Please choose another tile"
          playPlayerTurn gameState
