{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Concurrent.MVar
import Data.Array
import Data.Char (toLower)
import Graphics.UI.Gtk

import Game

data GameSetup = GameSetup { gameState :: MVar GameState
                           , cellImages :: Matrix Image
                           , colorPixbufs :: Array PuyoColor Pixbuf
                           }

data GameInput = Tick | KeyPress | Whatever
                 deriving (Eq, Show)

-- Used to load cell images on startup
colorPixBuf :: PuyoColor -> IO Pixbuf
colorPixBuf c = pixbufNewFromFileAtScale (map toLower $ show c ++ ".png") 20 20 True

setCellImage :: GameSetup -> PuyoCell -> Image -> IO ()
setCellImage s cell image = case cell of 
                              Empty -> imageClear image
                              Volatile color -> showColor color
                              Stable color -> showColor color
    where showColor = imageSetFromPixbuf image . (!) (colorPixbufs s)

-- Used once in setup to put the images in the table...
tableAttachCell table s ix@(row,col) = tableAttach table (cellImages s ! ix) col (col + 1) row (row + 1) [Fill] [Fill] 0 0

updateDisplay s@(GameSetup stateV images _) = do
  state <- readMVar stateV
  mapM_ (\ix -> setCellImage s (state ! ix) (images ! ix)) $ indices state

-- A demo function for showing that something happens over time
toggleTopLeft :: GameSetup -> IO ()
toggleTopLeft (GameSetup s _ _) = 
    void $ modifyMVar s $ \grid -> do
        let newColor = case grid ! (0,0) of 
                         Stable Red -> Stable Green
                         _ -> Stable Red
            newGrid = grid // [((0,0), newColor)]
        return (newGrid, newGrid)

restartGame s = do
  newGame <- newGameState
  modifyMVar (gameState s) (\_ -> return (newGame, newGame))

-- TODO: A better version with cairo rendering.
-- Image drawing is just a bit of a hassle...

setupGame colorBufs images = do
  s <- newMVar $ listArray ((0,0), (gridHeight - 1, gridWidth - 1)) $ repeat (Stable Red)
  return $ GameSetup s
              (listArray ((0,0), (gridHeight - 1, gridWidth - 1)) images)
              (listArray (Red,Yellow) colorBufs)

main = do
  initGUI
  window <- windowNew
  imageTable <- tableNew gridWidth gridHeight True
  layout <- tableNew 3 2 False

  labelLeft <- labelNew $ Just ("Left" :: String)
  labelRight <- labelNew $ Just ("Right" :: String)
  newGameButton <- buttonNewWithLabel ("New Game" :: String)
  tableAttachDefaults layout labelLeft 0 1 0 1
  tableAttachDefaults layout imageTable 1 2 0 1
  tableAttachDefaults layout newGameButton 0 3 1 2
  tableAttachDefaults layout labelRight 2 3 0 2
  
  colorBufs <- mapM colorPixBuf [Red,Green,Blue,Yellow]

  images <- mapM (const imageNew) [1..gridWidth * gridHeight]

  -- s is sort of a hack to get everything to everywhere.
  s <- setupGame colorBufs images

  mapM_ (tableAttachCell imageTable s) $ indices (cellImages s)

  onClicked newGameButton $ void $ restartGame s >> updateDisplay s

  updateDisplay s

  -- Pretend to do something over time - set a timer and toggle a cell
  t <- timeoutAdd (toggleTopLeft s >> updateDisplay s >> return True) 1000

  set window [ windowDefaultHeight := 400, windowDefaultWidth := 200,
               containerChild := layout ]
  onDestroy window mainQuit
  widgetShowAll window
  mainGUI
