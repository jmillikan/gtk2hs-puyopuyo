module Game (PuyoColor(..), PuyoCell(..), gridWidth, gridHeight, Matrix, GameState, newGameState) where

import Data.Array
import Data.Ix

data PuyoColor = Red | Green | Blue | Yellow
               deriving (Eq, Ix, Ord, Show)
data PuyoCell = Empty | Volatile PuyoColor | Stable PuyoColor
               deriving (Eq, Show)


gridWidth, gridHeight :: Int
gridWidth = 6
gridHeight = 13

type Matrix a = Array (Int, Int) a
type GameState = Matrix PuyoCell

newGameState :: IO GameState
newGameState = return $ listArray ((0, 0), (gridHeight - 1, gridWidth - 1)) $ repeat Empty


                                                                                        
