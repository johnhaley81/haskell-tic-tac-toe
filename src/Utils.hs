module Utils
( (|>)
, clearScreen
) where

(|>) :: a -> (a -> b) -> b
(|>) a f = f a

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"
