{-# LANGUAGE FlexibleInstances #-}

module Game (newGameState, gameStep, getGrid, gridWidth, gridHeight, GameState, ModifyTimer(..), Matrix, Grid, PuyoColor(..), PuyoCell(..), GameInput(..)) where

import Control.Arrow
import Data.Maybe
import Data.Array
import Data.Ix
import Data.List
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

instance Arbitrary PuyoColor where
    arbitrary = elements [Red, Green, Yellow, Blue]

instance Arbitrary Block where
    arbitrary = do 
      color1 <- arbitrary
      color2 <- arbitrary
      return $ listArray ((0,0),(1,1)) [Just color1, Just color2, Nothing, Nothing]

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
                                        if inRange (bounds b) bCellPos && (b ! bCellPos) /= Nothing
                                        then (ix, b ! bCellPos)
                                        else (ix, p)
          flipY (y,x) = (gridHeight - y - 1,x)

(-%) (x,y) (z,w) = (x-z, y-w)
(+%) (x,y) (z,w) = (x+z, y+w)

newGameState :: IO (GameState, Int)
-- Hardcoded start - one red cell to verify we're going, and a timeout
newGameState = do
  let blocks = blockQueue 42 -- todo: randomness
  return $ (GameState (ControlBlock (head blocks) (12, 1)) (tail blocks) exGrid, dropTimeout)

attachBlock :: Block -> (Int, Int) -> Grid -> Grid 
-- This is dumb. it turns out an Array of Maybes/other sums isn't great for
-- what I'm doing. (Or I'm using them very wrong.)
attachBlock b bix grid = grid // map ((bix +%) &&& Volatile . fromJust . (b !)) (occupiedIxs b)

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

droppablePuyos :: Grid -> Bool
droppablePuyos grid = any (ixDroppable grid) $ indices grid 

ixDroppable :: Grid -> (Int, Int) -> Bool
ixDroppable grid ix = inRange (bounds grid) (dropIx ix) && Empty /= grid ! ix && Empty == grid ! dropIx ix

dropIx :: (Int, Int) -> (Int, Int)
dropIx = first (\x -> x - 1)

-- iterate ixDroppable by removing droppables until it reaches a fixed point
dropPuyos :: Grid -> Grid
dropPuyos grid = (grid // [(ix, Empty) | ix <- allDroppable]) // [(dropIx ix, makeVolatile $ grid ! ix) | ix <- allDroppable]
   -- This is... Not efficient?
    where moreDroppable newGrid droppable = 
              let newDroppable = filter (ixDroppable newGrid) $ indices newGrid in
              if newDroppable == [] 
              then droppable 
              else moreDroppable (newGrid // [(ix, Empty) | ix <- newDroppable]) (droppable ++ newDroppable)
--          allDroppable = filter (ixDroppable grid) $ indices grid
          allDroppable = moreDroppable grid []
          makeVolatile cell = case cell of
                                Stable c -> Volatile c
                                Volatile c -> Volatile c
                                Empty -> Empty

removeablePuyos :: Grid -> Bool
removeablePuyos grid = not $ null $ removableGroups grid

removableGroups grid = filter ((> 3) . length) $ map (floodColor . (\x -> [x])) $ volatileIxs grid
    -- Brute force, not a proper search.
    where floodColor ixs@(ix:_) = let newIxs = nub $ (++) ixs $ filter (sameColorIx ix) $ concat $ map cross ixs
                                  in if newIxs == ixs then ixs 
                                     else floodColor newIxs
          cross ix = filter (inRange $ bounds grid) $ map (+% ix) [(1,0),(-1,0),(0,1),(0,-1)]
          sameColorIx ix ix2 = sameColor (grid ! ix) (grid ! ix2)
          sameColor Empty _ = False
          sameColor _ Empty = False
          sameColor c1 c2 = colorAt c1 == colorAt c2
          colorAt (Stable c) = c
          colorAt (Volatile c) = c

volatileIxs :: Grid -> [(Int, Int)]
volatileIxs grid = filter (volatile . (grid !)) $ indices grid
    where volatile (Volatile c) = True
          volatile _ = False

breakPuyos :: Grid -> Grid
breakPuyos grid = grid // [(ix, Empty) | ixs <- removableGroups grid, ix <- ixs]

-- Currently only works on our 2x2 blocks.
rotate :: Block -> Bool -> Block
rotate bl rotLeft = let [a,b,c,d] = elems bl in
                         listArray ((0,0),(1,1)) $ if rotLeft then [b,d,a,c] else [c,a,d,b]


-- This is a bit thick to avoid destructuring GameState in a lot of places.
gameStep :: GameState -> GameInput -> (GameState, ModifyTimer)
gameStep (GameState Cascading queue@(b:bs) grid) Tick
    -- Right now, the speed of these is locked to the tick since I haven't botherered making any intermediate states.
    | droppablePuyos grid  = (GameState Cascading queue (dropPuyos grid), Nothing)
    | removeablePuyos grid = (GameState Cascading queue (breakPuyos grid), Nothing)
    | otherwise            = (GameState (ControlBlock b (12,1)) bs grid, Just dropTimeout)
gameStep g@(GameState (ControlBlock b bix) queue grid) input 
    | cascading g = (g, Nothing) -- Ignore input while cascading
    | otherwise   = case input of
                      Tick -> tryDrop
                      Game.Left -> tryMove (0,-1)
                      Game.Right -> tryMove (0,1)
                      Game.Down -> tryDrop
                      RotLeft -> tryRotate True
                      RotRight -> tryRotate False
    where tryRotate left = tryChange (rotate b left) (0,0) Nothing noChange
          tryMove mv     = tryChange b mv Nothing noChange
          tryDrop        = tryChange b (-1,0) (Just dropTimeout) toCascade

          -- Try a move/rotate. If successful, "keep it" with the timer change. If failed, return fail.
          tryChange newB ixChange succeedTimerChange fail
              | movePossible grid newB bix ixChange = 
                  (GameState (ControlBlock newB (bix +% ixChange)) queue grid, succeedTimerChange)
              | otherwise = fail
          noChange = (g, Nothing)
          toCascade = (GameState Cascading queue (attachBlock b bix grid), Just cascadeTimeout)

