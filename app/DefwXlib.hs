module DefwXlib where

import           Data.Bits
import           DefwDefinition
import           Graphics.X11.Xlib
import           Graphics.X11.Xlib.Extras

mainLoop :: Display -> [DefwToken] -> Bool -> IO ()
mainLoop _ _ False = return ()
mainLoop d cmds True =
  getNextEvent d
    >>= runCycle cmds
    >>= mainLoop d cmds

getNextEvent :: Display -> IO Event
getNextEvent d = allocaXEvent $ \e ->
  nextEvent d e
    >> getEvent e

runCycle :: [DefwToken] -> Event -> IO Bool
runCycle = handleEvent

handleEvent :: [DefwToken] -> Event -> IO Bool
handleEvent _ (KeyEvent {ev_keycode = key}) = case key of
  24 -> return False
  _  -> return True
handleEvent cmds (ExposeEvent {ev_window = w, ev_event_display = d}) =
  drawWindowElements d w cmds
  >> return True
handleEvent _ _ = return True

drawWindowElements :: Display -> Window -> [DefwToken] -> IO ()
drawWindowElements d w (x:xs) =
  createDrawGc d w (arguments x)
  >>= drawElements
  where
    drawElements gc = drawCommands (commands x) gc
    drawCommands (x:xs) gc
      | null xs = drawCommand x gc
      | otherwise = drawCommand x gc >> drawCommands xs gc
    drawCommand x gc =
      case (commandName x) of
        "rect" -> drawRectangle d w gc
          (fromIntegral $ fst $ getAt $ commandData x)
          (fromIntegral $ snd $ getAt $ commandData x)
          (fromIntegral $ fst $ getSized $ commandData x)
          (fromIntegral $ snd $ getSized $ commandData x)
    getAt (x:xs) = case x of
      DefwAt r -> r
      _        -> getAt xs
    getSized (x:xs) = case x of
      DefwSized r -> r
      _           -> getSized xs


createDrawGc :: Display -> Window -> [DefwArgument] -> IO GC
createDrawGc d w args = do
  gc <- createGC d w
  interpretArguments args d gc
  return gc
  where
    interpretArguments (x:xs) d gc
      | null xs = stablishGcProperty x d gc
      | otherwise = stablishGcProperty x d gc >> interpretArguments xs d gc
    stablishGcProperty proper d gc =
      case (fst proper) of
        "fgColor" -> setForeground d gc (whitePixel d 0)

interpretCommands :: Display -> Window -> [DefwToken] -> IO ()
interpretCommands d w (x:xs)
  | null xs = interpretCommand x
  | otherwise = interpretCommand x >> interpretCommands d w xs
  where
    interpretCommand command = case command of
      DefwTitle title -> storeName d w title
      _               -> return ()

collectDrawInstructions :: [DefwToken] -> [DefwToken]
collectDrawInstructions = filter isDraw
  where
    isDraw (DefwDraw {}) = True
    isDraw _             = False

defwXlibMain :: DefwToken -> IO ()
defwXlibMain win = do
  d <- openDisplay ""
  w <- createSimpleWindow d (defaultRootWindow d) 0 0 500 500 3 (whitePixel d 0) (blackPixel d 0)

  mapWindow d w
  selectInput d w (keyPressMask .|. exposureMask)

  interpretCommands d w $ windowCommands win
  mainLoop d (collectDrawInstructions $ windowCommands win) True

  destroyWindow d w
  closeDisplay d
