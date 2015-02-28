import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent.MVar
import Data.Array
import Data.Tuple (swap)
import Data.Char (toLower)
import Graphics.UI.Gtk
import System.Glib.UTFString

import Game
import qualified Game as G (GameInput(..))

-- If there's a game running, there will be a tick timer running and an abstract state of the game.
data GameSetup = GameSetup { gameState :: MVar (Maybe (GameState, HandlerId))
                           , cellImages :: Array (Int, Int) Image
                           , colorPixbufs :: Array PuyoColor Pixbuf
                           , emptyPixbuf :: Pixbuf
                           }

-- Used to load cell images on startup
colorPixBuf :: PuyoColor -> IO Pixbuf
colorPixBuf c = pixbufNewFromFileAtScale (map toLower $ show c ++ ".png") 20 20 True

setCellImage :: GameSetup -> (Maybe PuyoColor) -> Image -> IO ()
setCellImage g cell image = imageSetFromPixbuf image $
                            case cell of 
                              Nothing -> emptyPixbuf g
                              Just color -> colorPixbufs g ! color

tableAttachCell table s ix@(row,col) = tableAttach table (cellImages s ! ix) col (col + 1) row (row + 1) [Fill] [Fill] 0 0

updateDisplay :: GameSetup -> IO ()
updateDisplay g@(GameSetup stateV images _ _) = do
  st <- readMVar stateV
  case st of Nothing -> return ()
             Just (state, _) -> do
               mapM_ (\(ix, c) -> setCellImage g c (images ! ix)) $ getGrid state

restartGame :: GameSetup -> IO ()
restartGame g = do
  (state, startTimer) <- newGameState
  newTimer <- timeoutAdd (runInput g Tick >> return True) startTimer
  modifyMVar_ (gameState g) $ const $ return $ Just (state, newTimer)

updateTimer :: GameSetup -> HandlerId -> Maybe Int -> IO HandlerId
updateTimer g oldTimer timerChange = 
    case timerChange of 
      Nothing -> return oldTimer
      Just n -> do 
        timeoutRemove oldTimer
        timeoutAdd (runInput g Tick >> return True) n

runInput :: GameSetup -> GameInput -> IO ()
runInput s input = do 
  modifyMVar_ (gameState s) $ \oldState -> 
      case oldState of Nothing -> error "Shouldn't tick while not running."
                       Just (oldState, oldTimer) -> do
                           let (newState, timerChange) = gameStep oldState input -- game logic call
                           newTimer <- updateTimer s oldTimer timerChange
                           return $ Just (newState, newTimer)
  updateDisplay s

keyNameToInput :: String -> Maybe GameInput
keyNameToInput k = case k of
                     "Left" -> Just G.Left
                     "Down" -> Just G.Down
                     "Right" -> Just G.Right
                     "a" -> Just G.Left
                     "s" -> Just G.Down
                     "d" -> Just G.Right
                     "Control_L" -> Just G.RotLeft
                     "Alt_L" -> Just G.RotRight
                     "Alt_R" -> Just G.RotLeft
                     "Control_R" -> Just G.RotRight
                     _ -> Nothing

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

  emptyBuf <- pixbufNewFromFileAtScale "empty.png" 20 20 True

  st <- newMVar Nothing
  let g = GameSetup st
             (listArray ((0,0), (gridHeight - 1, gridWidth - 1)) images)
             (listArray (Red,Yellow) colorBufs)
             emptyBuf

  -- Todo: Have our own state where we show something nice while the game isn't running
  -- Todo: Decide how we store and display "between games"

  mapM_ (tableAttachCell imageTable g) $ indices (cellImages g)

  onClicked newGameButton $ void $ restartGame g >> updateDisplay g

  window `on` keyPressEvent $ tryEvent $ do
         keyVal <- eventKeyVal
         case keyNameToInput $ glibToString $ keyName keyVal of
           Nothing -> return ()
           Just input -> liftIO $ runInput g input

  --t <- timeoutAdd (toggleTopLeft s >> updateDisplay s >> return True) 1000

  set window [ windowDefaultHeight := 400, windowDefaultWidth := 200,
               containerChild := layout ]
  onDestroy window mainQuit
  widgetShowAll window
  mainGUI
