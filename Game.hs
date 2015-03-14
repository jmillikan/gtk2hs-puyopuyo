{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Game (newGameState, gameStep, getGrid, getScore, gridWidth, gridHeight, GameState, PuyoColor(..), GameInput(..)) where

import Control.Lens
import Control.Arrow
import Control.Applicative
import qualified Data.Array as A
import Data.Array ((!), Array, listArray, (//), bounds, assocs)
import Data.Ix
import Data.List (nub)
import System.Random
import qualified Test.QuickCheck.Gen as G
import Test.QuickCheck.Random
import Test.QuickCheck.Arbitrary

-- Graphics bits use Ix. This shouldn't expose that though...
data PuyoColor = Red | Green | Blue | Yellow
               deriving (Eq, Ix, Ord, Show)
type PuyoCell = Maybe (Bool, PuyoColor)
data GameInput = Tick | Down | Left | Right | RotLeft | RotRight
               deriving (Eq, Show)

-- This should be a map. I'm lazy.
newtype Block = Block { cells :: [((Int, Int), PuyoColor)] }

-- Either "cascading" (blocks dropping/popping) or controlling a 2x2 block with bottom left at (x,y)
--data BaseState = ControlBlock Block (Int, Int) | Cascading | Over

-- Column major, starts at bottom-left
type Grid = Matrix PuyoCell

type Matrix a = Array (Int, Int) a

data GameState = Cascade { _pieceQueue :: [Block]
                         , _gameGrid :: Grid
                         }
               | ControlBlock   { _pieceQueue :: [Block]
                                , _gameGrid :: Grid
                                , _block :: (Block, (Int, Int))
                                }
               | GameOver { _pieceQueue :: [Block]
                          , _gameGrid :: Grid
                          }

makeLenses ''GameState

-- Used for ad-hoc testing... and as the current start board
exGrid = listArray ((0, 0), (gridHeight - 1, gridWidth - 1)) $ Just (True, Red) : repeat Nothing

instance Arbitrary PuyoColor where
    arbitrary = G.elements [Red, Green, Yellow, Blue]

instance Arbitrary Block where
    arbitrary = makeBlock <$> arbitrary <*> arbitrary

makeBlock c1 c2 = Block [((0,0), c1), ((1,0), c2)]

gridHeight, gridWidth :: Int
(gridHeight, gridWidth) = (13,6)

cascadeTimeout, dropTimeout :: Int
cascadeTimeout = 600
dropTimeout = 1000

type ModifyTimer = Maybe Int

blockQueue :: Int -> [Block]
blockQueue seed = G.unGen infiniteList (mkQCGen seed) seed 

getGrid :: GameState -> [((Int, Int), Maybe PuyoColor)]
getGrid state = toCellList $ maybe id attachBlock (state^?block) $ state^.gameGrid
  
getScore :: GameState -> Int
getScore _ = 9999

toCellList :: Grid -> [((Int, Int), Maybe PuyoColor)]
toCellList g = map (flipY *** color) $ A.assocs g
    where color = preview (traverse . _2)
          flipY (y,x) = (gridHeight - y - 1,x)

(+%) (x,y) (z,w) = (x+z, y+w)

attachBlock :: (Block, (Int, Int)) -> Grid -> Grid 
attachBlock b grid = grid // (map (second volatileCell) $ blockPairs b)
    where volatileCell c = Just (False, c)

positionPossible :: Grid -> (Block, (Int, Int)) -> Bool
positionPossible grid b =
    all (inRange (bounds grid)) bixsOnGrid &&
    all ((Nothing ==) . (grid !)) bixsOnGrid
    where bixsOnGrid = map fst $ blockPairs b

blockPairs :: (Block, (Int, Int)) -> [((Int, Int), PuyoColor)]
blockPairs (b, bix) = map (first (+% bix)) $ cells b

droppablePuyos :: Grid -> Bool
droppablePuyos grid = any (cellDroppable grid) $ A.assocs grid 

cellDroppable :: Grid -> ((Int, Int), PuyoCell) -> Bool
cellDroppable _ (ix, Nothing) = False
cellDroppable grid (ix, _)  = inRange (bounds grid) (dropIx ix) &&
                              Nothing == grid ! dropIx ix

dropIx :: (Int, Int) -> (Int, Int)
dropIx = _1 %~ (\x -> x - 1)

dropCell :: ((Int, Int), PuyoCell) -> ((Int, Int), PuyoCell)
dropCell = _1 %~ dropIx

-- Does 1 "step" of dropping - everything possible falls one space only.
dropPuyos :: Grid -> Grid
dropPuyos = dropMore []
    where dropMore droppedIxs grid = 
              let moreIxs = filter (droppable grid) $ A.assocs grid in
              if moreIxs == [] then grid
              else dropMore (droppedIxs ++ map dropCell moreIxs) $ (grid // [(ix, Nothing) | (ix, _) <- moreIxs])
                   // [(dropIx ix, setVolatile cell) | (ix, cell) <- moreIxs]
              where droppable grid cell = cellDroppable grid cell && not (elem cell droppedIxs)
                    setVolatile = set (traverse._1) False

removeablePuyos :: Grid -> Bool
removeablePuyos grid = not $ null $ removableGroups grid

removableGroups grid = filter ((> 3) . length) $ map (floodColor . (\(ix,_) -> [ix])) $ filter (volatile . snd) $ A.assocs grid
    -- Brute force, not a proper search.
    where floodColor ixs@(ix:_) = let newIxs = nub $ (++) ixs $ filter (sameColorIx ix) $ concat $ map cross ixs
                                  in if newIxs == ixs then ixs 
                                     else floodColor newIxs
          cross ix = filter (inRange $ bounds grid) $ map (+% ix) [(1,0),(-1,0),(0,1),(0,-1)]
          sameColorIx ix ix2 = (sameColor <$> (grid ! ix) <*> (grid ! ix2)) == Just True
          sameColor (_,c) (_,c2) = c == c2
          volatile (Just (False,_)) = True
          volatile _                = False

breakPuyos :: Grid -> Grid
breakPuyos grid = grid // [(ix, Nothing) | ixs <- removableGroups grid, ix <- ixs]

-- Currently only works on our 2x2 blocks.
rotate :: Bool -> Block -> Block
rotate rotLeft bl = Block $ map (first trans) $ cells bl
    where trans (y,x) = (if rotLeft then 1 - x else x, if rotLeft then y else 1 - y)

newGameState :: IO (GameState, Int)
newGameState = do
  let blocks = blockQueue 42 -- todo: randomness
  return $ (ControlBlock (tail blocks) exGrid (head blocks, (11, 1)), dropTimeout)

-- This is a mess but does cover everything (Cascading + tick, Cascading + other, ControlBlock + all)
gameStep :: GameState -> GameInput -> (GameState, ModifyTimer)
gameStep g@(GameOver _ _) _ = (g, Nothing)
gameStep g@(Cascade queue@(b:bs) grid) Tick
    -- Right now, the speed of these is locked to the tick since I haven't botherered making any intermediate states.
    | droppablePuyos grid  = (gameGrid %~ dropPuyos $ g, Nothing)
    | removeablePuyos grid = (gameGrid %~ breakPuyos $ g, Nothing)
    | otherwise            = if positionPossible grid (b, (11, 1))
                             then (ControlBlock bs grid (b, (11,1)), Just dropTimeout)
                             else (GameOver queue grid, Nothing)
gameStep g@(Cascade _ _) _ = (g, Nothing) -- Ignore input while cascading
gameStep g@(ControlBlock queue grid bl) input = 
    case input of
      Tick -> tryDrop
      Game.Left -> tryMove (0,-1)
      Game.Right -> tryMove (0,1)
      Game.Down -> tryDrop
      RotLeft -> tryRotate True
      RotRight -> tryRotate False
    where tryRotate left = tryChange (_1 %~ rotate left) Nothing noChange
          tryMove mv     = tryChange (_2 %~ (+%) mv) Nothing noChange
          tryDrop        = tryChange (_2._1 -~ 1) (Just dropTimeout) toCascade
          -- Try a move/rotate. If successful, "keep it" with the timer change. If failed, return fail.
          tryChange change succeedTimerChange fail
              | positionPossible grid (change bl) =
                  (ControlBlock queue grid (change bl), succeedTimerChange)
              | otherwise = fail
          noChange = (g, Nothing)
          toCascade = (Cascade queue (attachBlock bl grid), Just cascadeTimeout)
