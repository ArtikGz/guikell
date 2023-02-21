module DefwXlib where

import Data.Bits
import DefwDefinition
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras

runCycle :: Event -> IO Bool
runCycle = handleEvent

handleEvent :: Event -> IO Bool
handleEvent (KeyEvent {ev_keycode = key}) = case key of
  24 -> return False
  _ -> return True
handleEvent (ExposeEvent {ev_window = w, ev_width = width, ev_height = height, ev_event_display = d}) =
  drawWindowElements d w (fromIntegral width) (fromIntegral height)
    >> return True

drawWindowElements :: Display -> Window -> Int -> Int -> IO ()
drawWindowElements d w width height =
  createGC d w >>= drawElement
  where
    drawElement gc =
      setForeground d gc (whitePixel d 0)
        >> drawRectangle d w gc 50 50 50 50
        >> drawRectangle d w gc 50 150 50 50
        >> drawString d w gc 50 125 "Привет, мир"

getNextEvent :: Display -> IO Event
getNextEvent d = allocaXEvent $ \e ->
  nextEvent d e
    >> getEvent e

mainLoop :: Display -> Bool -> IO ()
mainLoop _ False = return ()
mainLoop d True =
  getNextEvent d
    >>= runCycle
    >>= mainLoop d

interpretCommands :: Display -> Window -> [DefwToken] -> IO ()
interpretCommands d w (x:xs)
  | null xs = interpretCommand x
  | otherwise = interpretCommand x >> interpretCommands d w xs
  where
    interpretCommand command = case command of
      DefwTitle title -> storeName d w title

defwXlibMain :: DefwToken -> IO ()
defwXlibMain win = do
  d <- openDisplay ""
  w <- createSimpleWindow d (defaultRootWindow d) 0 0 500 500 3 (whitePixel d 0) (blackPixel d 0)

  mapWindow d w
  selectInput d w (keyPressMask .|. exposureMask)

  interpretCommands d w $ windowCommands win
  mainLoop d True

  destroyWindow d w
  closeDisplay d