{-# LANGUAGE FlexibleInstances #-}

module Game (newGameState, gameStep, getGrid, gridWidth, gridHeight, GameState, ModifyTimer(..), Matrix, Grid, PuyoColor(..), PuyoCell(..), GameInput(..)) where

import Control.Arrow
import Data.Maybe
import Data.Array
import Data.Ix
import System.Random
import Test.QuickCheck.Gen
import Test.QuickCheck.Random
import Test.QuickCheck.Arbitrary

-- Graphics bits use Ix. This shouldn't expose that though...
data PuyoColor = Red | Green | Blue | Yellow
               deriving (Eq, Ix, Ord, Show)
data PuyoCell = Empty | Volatile PuyoColor | Stable PuyoColor
               deriving (Eq, Show)
data GameInput = Tick | Down | Left | Right | RotLeft | RotRight
               deriving (Eq, Show)
type Block = Matrix (Maybe PuyoColor)

-- Either "cascading" (blocks dropping/popping) or controlling a 2x2 block with bottom left at (x,y)
data BaseState = ControlBlock Block (Int, Int) | Cascading

-- Column major, starts at bottom-left
type Grid = Matrix PuyoCell

type Matrix a = Array (Int, Int) a
data GameState = GameState { baseState :: BaseState
                           , pieceQueue :: [Block]
                           , gameGrid :: Grid
                           }

exBlock = listArray ((0,0), (1,1)) [Just Red, Just Green, Nothing, Nothing]

exGrid = listArray ((0, 0), (gridHeight - 1, gridWidth - 1)) $ Stable Red : repeat Empty

instance Arbitrary Block where
    arbitrary = elements [ exBlock ] -- TODO

gridWidth, gridHeight :: Int
gridWidth = 6
gridHeight = 13

cascadeTimeout, dropTimeout :: Int
cascadeTimeout = 600
dropTimeout = 1000

newBlock :: IO Block
newBlock = return exBlock

cascading (GameState Cascading _ _) = True
cascading _ = False

type ModifyTimer = Maybe Int

blockQueue :: Int -> [Block]
blockQueue seed = unGen infiniteList (mkQCGen seed) seed 

-- "translate" our game state into a visible grid including current block
getGrid :: GameState -> [((Int, Int), Maybe PuyoColor)]
getGrid g = map (first flipY . overlayBlock . justColors) (indices $ gameGrid g)
    where justColors ix = case gameGrid g ! ix of 
                           Empty -> (ix, Nothing)
                           Stable c -> (ix, Just c)
                           Volatile c -> (ix, Just c)
          overlayBlock (ix, p) = case g of
                                   (GameState Cascading _ _) -> (ix, p)
                                   (GameState (ControlBlock b bix) _ _) ->
                                       -- map ix to inside b...
                                        let bCellPos = ix -% bix in
                                        if inRange (bounds b) bCellPos
                                        then (ix, b ! bCellPos)
                                        else (ix, p)
          flipY (y,x) = (gridHeight - y - 1,x)

(-%) (x,y) (z,w) = (x-z, y-w)
(+%) (x,y) (z,w) = (x+z, y+w)

newGameState :: IO (GameState, Int)
-- Hardcoded start - one red cell to verify we're going, and a timeout
newGameState = do
  let blocks = blockQueue 42 -- todo: randomness
  return $ (GameState (ControlBlock (head blocks) (10, 1)) (tail blocks) exGrid, dropTimeout)

attachBlock :: Block -> (Int, Int) -> Grid -> Grid 
-- This is dumb. it turns out an Array of Maybes/other sums isn't great for
-- what I'm doing. (Or I'm using them very wrong.)
attachBlock b bix grid = grid // map ((bix +%) &&& Volatile . fromJust . (b !)) (occupiedIxs b)

-- TODO: Most of these should not take or return all of GameState nor return all of (GameState, ModifyTimer). 
-- They should do more specific transforms, and information about timers should probably go into gamestep or helpers
-- Probably rework this by having a single function tryNewPos

-- Try a left/right move. Not sure why I took the whole ix...
-- TODO: Merge with tryDrop, branching in here on whether there's y-change
tryMove :: GameState -> (Int, Int) -> (GameState, ModifyTimer)
tryMove g@(GameState (ControlBlock b ix) queue grid) mv 
        | movePossible grid b ix mv = (GameState (ControlBlock b (ix +% mv)) queue grid, Nothing)
        | otherwise = (g, Nothing)

tryDrop :: GameState -> (GameState, ModifyTimer)
tryDrop g@(GameState (ControlBlock b bix) queue grid) 
        | movePossible grid b bix (-1,0) = (GameState (ControlBlock b (bix +% (-1,0))) queue grid, Just dropTimeout)
        | otherwise = (GameState Cascading queue (attachBlock b bix grid), Just cascadeTimeout)

tryRotate :: GameState -> Bool -> (GameState, ModifyTimer)
tryRotate g@(GameState (ControlBlock b ix) queue grid) rotLeft
    | movePossible grid (rotateBlock b rotLeft) ix (0,0) = (GameState (ControlBlock (rotateBlock b rotLeft) ix) queue grid, Nothing)
    | otherwise = (g, Nothing)

-- TODO: Write this for any square bounds, though in this game they're all ((0,0),(1,1))
rotateBlock :: Block -> Bool -> Block
rotateBlock bl rotLeft = let [a,b,c,d] = elems bl in
                        listArray ((0,0),(1,1)) $ if rotLeft then [b,d,a,c] else [c,a,d,b]

-- TODO: Refactor to be "positionPossible" without mv?
movePossible :: Grid -> Block -> (Int, Int) -> (Int, Int) -> Bool
movePossible grid b bix mv = 
    let newIx = bix +% mv
        bixs = occupiedIxs b in -- bix is outside, bixs is inside >_<
    -- Check each occupied cell (index) of the block in its new position
    -- for being inside the grid and not occupied
    all (inRange (bounds grid) . (newIx +%)) bixs && all (notOccupied . (newIx +%)) bixs
    where notOccupied ix = Empty == (grid ! ix)

occupiedIxs b = filter (not . (== Nothing) . (b !)) (indices b)

continueCascade :: GameState -> (GameState, ModifyTimer)
-- For now, just terminate cascade after 1 tick...
continueCascade (GameState Cascading (b:bs) grid)
    | droppablePuyos grid  = (GameState Cascading (b:bs) (dropPuyos grid), Nothing)
    | removeablePuyos grid = (GameState Cascading (b:bs) (breakPuyos grid), Nothing)
    | otherwise            = (GameState (ControlBlock b (12,1)) bs grid, Just dropTimeout)

droppablePuyos :: Grid -> Bool
droppablePuyos grid = any (ixDroppable grid) $ indices grid 

ixDroppable :: Grid -> (Int, Int) -> Bool
ixDroppable grid ix = inRange (bounds grid) (dropIx ix) && Empty /= grid ! ix && Empty == grid ! dropIx ix

dropIx :: (Int, Int) -> (Int, Int)
dropIx = first (\x -> x - 1)

-- iterate ixDroppable by removing droppables until it reaches a fixed point
dropPuyos :: Grid -> Grid
dropPuyos grid = (grid // [(ix, Empty) | ix <- allDroppable]) // [(dropIx ix, grid ! ix) | ix <- allDroppable]
   -- This is... Not efficient?
    where moreDroppable newGrid droppable = 
              let newDroppable = filter (ixDroppable newGrid) $ indices newGrid in
              if newDroppable == [] 
              then droppable 
              else moreDroppable (newGrid // [(ix, Empty) | ix <- newDroppable]) (droppable ++ newDroppable)
--          allDroppable = filter (ixDroppable grid) $ indices grid
          allDroppable = moreDroppable grid []

removeablePuyos :: Grid -> Bool
removeablePuyos grid = False

breakPuyos :: Grid -> Grid
breakPuyos = undefined

-- TODO: We really should just break down the gamestate here and fix the
-- function signatures so they aren't incomplete...
gameStep :: GameState -> GameInput -> (GameState, ModifyTimer)
gameStep g Tick 
    | cascading g = continueCascade g
    | otherwise   = tryDrop g
gameStep g input 
    | cascading g = (g, Nothing) -- Ignore input while cascading
    | otherwise   = case input of
                      Game.Left -> tryMove g (0,-1)
                      Game.Right -> tryMove g (0,1)
                      Game.Down -> tryDrop g
                      RotLeft -> tryRotate g True
                      RotRight -> tryRotate g False
                      _ -> (g, Nothing) -- Ignore other inputs (including Tick because I'm back)
