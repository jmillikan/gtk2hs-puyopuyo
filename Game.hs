{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Game (newGameState, gameStep, getGrid, gridWidth, gridHeight, GameState, PuyoColor(..), GameInput(..)) where

import Control.Lens
import Control.Arrow
import Data.Maybe
import qualified Data.Array as A
import Data.Array ((!), Array, listArray, elems, (//), bounds, assocs)
import Data.Ix
import Data.List
import System.Random
import qualified Test.QuickCheck.Gen as G
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
data GameState = GameState { _baseState :: BaseState
                           , _pieceQueue :: [Block]
                           , _gameGrid :: Grid
                           }

makeLenses ''GameState

-- Used for ad-hoc testing...
exBlock = listArray ((0,0), (1,1)) [Just Red, Just Green, Nothing, Nothing]

exGrid = listArray ((0, 0), (gridHeight - 1, gridWidth - 1)) $ Stable Red : repeat Empty

instance Arbitrary PuyoColor where
    arbitrary = G.elements [Red, Green, Yellow, Blue]

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

type ModifyTimer = Maybe Int

blockQueue :: Int -> [Block]
blockQueue seed = G.unGen infiniteList (mkQCGen seed) seed 

getGrid :: GameState -> [((Int, Int), Maybe PuyoColor)]
getGrid (GameState Cascading _ grid) = toCellList grid
-- attachBlock is convenient but adds "junk data"
getGrid (GameState (ControlBlock b bix) _ grid) = toCellList $ attachBlock b bix grid

toCellList :: Grid -> [((Int, Int), Maybe PuyoColor)]
toCellList g = map (flipY *** color) $ A.assocs g
    where color Empty = Nothing
          color (Stable c) = Just c
          color (Volatile c) = Just c
          flipY (y,x) = (gridHeight - y - 1,x)

(-%) (x,y) (z,w) = (x-z, y-w)
(+%) (x,y) (z,w) = (x+z, y+w)

newGameState :: IO (GameState, Int)
-- Hardcoded start - one red cell to verify we're going, and a timeout
newGameState = do
  let blocks = blockQueue 42 -- todo: randomness
  return $ (GameState (ControlBlock (head blocks) (12, 1)) (tail blocks) exGrid, dropTimeout)

-- It's a bit dumb that the piece manipulation and cascade share so little.

attachBlock :: Block -> (Int, Int) -> Grid -> Grid 
-- This is dumb. it turns out an Array of Maybes/other sums isn't great for
-- what I'm doing. (Or I'm using them very wrong.)
attachBlock b bix grid = grid // map ((bix +%) *** (Volatile . fromJust)) 
                         (filter (isJust . snd) (assocs b))

positionPossible :: Grid -> Block -> (Int, Int) -> Bool
positionPossible grid b newIx = 
    all (inRange (bounds grid)) bixsOnGrid && 
    all ((Empty ==) . (grid !)) bixsOnGrid
    where bixsOnGrid = map (+% newIx) $ filter (not . (== Nothing) . (b !)) (A.indices b)

droppablePuyos :: Grid -> Bool
droppablePuyos grid = any (ixDroppable grid) $ A.indices grid 

ixDroppable :: Grid -> (Int, Int) -> Bool
ixDroppable grid ix = inRange (bounds grid) (dropIx ix) && 
                      Empty /= grid ! ix && 
                      Empty == grid ! dropIx ix

dropIx :: (Int, Int) -> (Int, Int)
dropIx = _1 -~ 1

dropPuyos :: Grid -> Grid
dropPuyos grid = let droppable = filter (ixDroppable grid) $ A.indices grid in
                  if droppable == [] then grid
                  else dropPuyos $ (grid // [(ix, Empty) | ix <- droppable]) // [(dropIx ix, makeVolatile $ grid ! ix) | ix <- droppable]
    where makeVolatile cell = case cell of
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
volatileIxs grid = filter (volatile . (grid !)) $ A.indices grid
    where volatile (Volatile c) = True
          volatile _ = False

breakPuyos :: Grid -> Grid
breakPuyos grid = grid // [(ix, Empty) | ixs <- removableGroups grid, ix <- ixs]

-- Currently only works on our 2x2 blocks.
rotate :: Block -> Bool -> Block
rotate bl rotLeft = let [a,b,c,d] = elems bl in
                         listArray ((0,0),(1,1)) $ if rotLeft then [b,d,a,c] else [c,a,d,b]

-- This is a mess but does cover everything (Cascading + tick, Cascading + other, ControlBlock + all)
gameStep :: GameState -> GameInput -> (GameState, ModifyTimer)
gameStep g@(GameState Cascading queue@(b:bs) grid) Tick
    -- Right now, the speed of these is locked to the tick since I haven't botherered making any intermediate states.
    | droppablePuyos grid  = (over gameGrid dropPuyos g, Nothing)
    | removeablePuyos grid = (over gameGrid breakPuyos g, Nothing)
    | otherwise            = (GameState (ControlBlock b (12,1)) bs grid, Just dropTimeout)
gameStep g@(GameState Cascading _ _) _ = (g, Nothing) -- Ignore input while cascading
gameStep g@(GameState (ControlBlock b bix) queue grid) input = 
    case input of
      Tick -> tryDrop
      Game.Left -> tryMove (0,-1)
      Game.Right -> tryMove (0,1)
      Game.Down -> tryDrop
      RotLeft -> tryRotate True
      RotRight -> tryRotate False
    where tryRotate left = tryChange (rotate b left) bix Nothing noChange
          tryMove mv     = tryChange b (bix +% mv) Nothing noChange
          tryDrop        = tryChange b (_1 -~ 1 $ bix) (Just dropTimeout) toCascade
          -- Try a move/rotate. If successful, "keep it" with the timer change. If failed, return fail.
          tryChange newB newBix succeedTimerChange fail
              | positionPossible grid newB newBix = 
                  (GameState (ControlBlock newB newBix) queue grid, succeedTimerChange)
              | otherwise = fail
          noChange = (g, Nothing)
          toCascade = (GameState Cascading queue (attachBlock b bix grid), Just cascadeTimeout)

