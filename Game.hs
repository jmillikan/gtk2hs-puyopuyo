module Game where

import Data.Array
import Data.Ix

-- Graphics bits use Ix. This shouldn't expose that though...
data PuyoColor = Red | Green | Blue | Yellow
               deriving (Eq, Ix, Ord, Show)
data PuyoCell = Empty | Volatile PuyoColor | Stable PuyoColor
               deriving (Eq, Show)
data GameInput = Tick | Down | Left | Right | RotLeft | RotRight
               deriving (Eq, Show)
type Block = ()
data BaseState = ControlBlock Block | Cascading

gridWidth, gridHeight :: Int
gridWidth = 6
gridHeight = 13

newBlock :: IO Block
newBlock = return ()

type Grid = Matrix PuyoCell

type Matrix a = Array (Int, Int) a
data GameState = GameState { baseState :: BaseState
                           , gameGrid :: Grid
                           }

type ModifyTimer = Maybe Int

newGameState :: IO (GameState, Int)
-- Hardcoded start - one red cell to verify we're going, and a timeout of 1000
newGameState = do
  block <- newBlock
  return $ (GameState (ControlBlock block) (listArray ((0, 0), (gridHeight - 1, gridWidth - 1)) $ Stable Red : repeat Empty), 1000)

tryDrop :: Grid -> Block -> (GameState, ModifyTimer)
tryDrop = undefined

continueCascade :: Grid -> (GameState, ModifyTimer)
continueCascade = undefined

gameStep :: GameState -> GameInput -> (GameState, ModifyTimer)
gameStep (GameState (ControlBlock b) grid) Tick = tryDrop grid b
gameStep (GameState Cascading grid) Tick = continueCascade grid
gameStep s@(GameState Cascading grid) _ = (s, Nothing)
gameStep s@(GameState (ControlBlock _) grid) _ = (s, Nothing) -- do nothing for now...
