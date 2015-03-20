{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Game (newGameState, gameStep, getGrid, getScore, gridWidth, gridHeight, GameState, PuyoColor(..), GameInput(..)) where

import Control.Lens hiding (elements)
import Control.Arrow
import Control.Applicative
import Control.Monad
import Data.Array ((!), Array, listArray, (//), bounds, assocs)
import Data.Ix
import Data.List (nub)
import System.Random
import Test.QuickCheck.Gen
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

-- Column major, starts at bottom-left
type Grid = Matrix PuyoCell

type Matrix a = Array (Int, Int) a

data GameState = Cascade { _pieceQueue :: [Block]
                         , _gameGrid :: Grid
                         , _score :: Int
                         , _scoreInc :: Int
                         }
               | ControlBlock { _pieceQueue :: [Block]
                              , _gameGrid :: Grid
                              , _block :: (Block, (Int, Int))
                              , _score :: Int
                              }
               | GameOver { _pieceQueue :: [Block]
                          , _gameGrid :: Grid
                          , _score :: Int
                          }

makeLenses ''GameState

-- Used for ad-hoc testing... and as the current start board
exGrid = listArray ((0, 0), (gridHeight - 1, gridWidth - 1)) $ Just (True, Red) : repeat Nothing

instance Arbitrary PuyoColor where
    arbitrary = elements [Red, Green, Yellow, Blue]

instance Arbitrary Block where
    arbitrary = makeBlock <$> arbitrary <*> arbitrary

makeBlock c1 c2 = Block [((0,0), c1), ((1,0), c2)]

gridHeight, gridWidth :: Int
(gridHeight, gridWidth) = (13,6)

cascadeTimeout, dropTimeout :: Int
cascadeTimeout = 600
dropTimeout = 1000

type ModifyTimer = Maybe Int

blockQueue :: Int -> Int -> [Block]
blockQueue seed seed2 = unGen infiniteList (mkQCGen seed) seed2 

getGrid :: GameState -> [((Int, Int), Maybe PuyoColor)]
getGrid state = toCellList $ maybe id attachBlock (state^?block) $ state^.gameGrid
  
getScore :: GameState -> Int
getScore g = g^.score

toCellList :: Grid -> [((Int, Int), Maybe PuyoColor)]
toCellList g = map (flipY *** (^? (traverse . _2))) $ assocs g
    where flipY (y,x) = (gridHeight - y - 1,x)

(+%) (x,y) (z,w) = (x+z, y+w)

attachBlock :: (Block, (Int, Int)) -> Grid -> Grid 
attachBlock b grid = grid // (map (_2 %~ volatileCell) $ blockPairs b)
    where volatileCell c = Just (False, c)

positionPossible :: Grid -> (Block, (Int, Int)) -> Bool
positionPossible grid b =
    all (inRange (bounds grid)) bixsOnGrid &&
    all ((Nothing ==) . (grid !)) bixsOnGrid
    where bixsOnGrid = map fst $ blockPairs b

blockPairs :: (Block, (Int, Int)) -> [((Int, Int), PuyoColor)]
blockPairs (b, bix) = map (_1 %~ (+% bix)) $ cells b

droppablePuyos :: Grid -> Bool
droppablePuyos grid = any (cellDroppable grid) $ assocs grid 

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
              let moreIxs = filter (droppable grid) $ assocs grid in
              if moreIxs == [] then grid
              else dropMore (droppedIxs ++ map dropCell moreIxs) $ 
                       (grid // ((traverse._2) .~ Nothing $ moreIxs))
                       // map (dropIx *** (traverse._1) .~ False) moreIxs
              where droppable grid cell = cellDroppable grid cell && not (elem cell droppedIxs)

removeablePuyos :: Grid -> Bool
removeablePuyos grid = not $ null $ removableGroups grid

removableGroups grid = filter ((> 3) . length) $ map (floodColor . (\(ix,_) -> [ix])) $ filter (volatile . snd) $ assocs grid
    -- Brute force, not a proper search.
    where floodColor ixs@(ix:_) = let newIxs = nub $ (++) ixs $ filter (sameColorIx ix) $ concat $ map cross ixs
                                  in if newIxs == ixs then ixs 
                                     else floodColor newIxs
          cross ix = filter (inRange $ bounds grid) $ map (+% ix) [(1,0),(-1,0),(0,1),(0,-1)]
          sameColorIx ix ix2 = Just True == (sameColor <$> grid ! ix <*> grid ! ix2)
          sameColor (_,c) (_,c2) = c == c2
          volatile (Just (False,_)) = True
          volatile _                = False

breakPuyos :: Grid -> (Grid, Int)
breakPuyos grid = (newGrid, length $ removableGroups grid)
    where newGrid = grid // [(ix, Nothing) | ix <- join $ removableGroups grid]

-- Currently only works on our 2x2 blocks.
rotate :: Bool -> Block -> Block
rotate rotLeft bl = Block $ map (_1 %~ trans) $ cells bl
    where trans (y,x) = (if rotLeft then 1 - x else x, if rotLeft then y else 1 - y)

newGameState :: IO (GameState, Int)
newGameState = do
  let blocks = blockQueue 42 4 -- todo: randomness
  return $ (ControlBlock (tail blocks) exGrid (head blocks, (11, 1)) 0, dropTimeout)

-- This is a mess but does cover everything (Cascading + tick, Cascading + other, ControlBlock + all)
gameStep :: GameState -> GameInput -> (GameState, ModifyTimer)
gameStep g@(GameOver _ _ _) _ = (g, Nothing)
gameStep g@(Cascade (b:bs) grid sc inc) Tick
    -- Right now, the speed of these is locked to the tick since I haven't botherered making any intermediate states.
    | droppablePuyos grid  = (gameGrid %~ dropPuyos $ g, Nothing)
    | removeablePuyos grid = let (newGrid, groupsBroken) = breakPuyos $ g^.gameGrid in
                             (scoreInc +~ (10 * groupsBroken) $ score +~ inc $ gameGrid .~ newGrid $ g, Nothing)
    | otherwise            = if positionPossible grid (b, (11, 1))
                             then (ControlBlock bs grid (b, (11,1)) sc, Just dropTimeout)
                             else (GameOver (b:bs) grid sc, Nothing)
gameStep g@(Cascade _ _ _ _) _ = (g, Nothing) -- Ignore input while cascading
gameStep g@(ControlBlock queue grid bl score) input = 
    case input of
      Tick       -> tryDrop
      Game.Left  -> tryShift (_2 %~ (+%) (0,-1)) 
      Game.Right -> tryShift (_2 %~ (+%) (0,1)) 
      Game.Down  -> tryDrop
      RotLeft    -> tryShift (_1 %~ rotate True)
      RotRight   -> tryShift (_1 %~ rotate False)
    where tryShift change = if positionPossible grid (change bl) 
                            then (ControlBlock queue grid (change bl) score, Nothing)
                            else (g, Nothing)
          tryDrop = if positionPossible grid (drop bl) 
                    then (ControlBlock queue grid (drop bl) score, (Just dropTimeout))
                    else (Cascade queue (attachBlock bl grid) score 10, Just cascadeTimeout)
          drop = (_2._1 -~ 1)
