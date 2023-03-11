{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module DefwXlib where

import           Control.Monad
import           Data.Bits
import           Data.Functor
import           DefwDefinition
import           Graphics.X11.Xlib
import           Graphics.X11.Xlib.Color
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
drawWindowElements d w args =
  forM_ args $ \arg ->
    createDrawGc d w (arguments arg)
      >>= drawElements (commands arg)
  where
    drawElements commands gc = forM_ commands $ \command ->
      case command of
        DefwCommand "rect" [DefwAt at, DefwSized sized] ->
          drawRectangle
            d
            w
            gc
            (fromIntegral $ fst at)
            (fromIntegral $ snd at)
            (fromIntegral $ fst sized)
            (fromIntegral $ snd sized)
        DefwCommand "text" [DefwAt at, DefwAs as] ->
          drawString
            d
            w
            gc
            (fromIntegral $ fst at)
            (fromIntegral $ snd at)
            as

createDrawGc :: Display -> Window -> [DefwArgument] -> IO GC
createDrawGc d w args = do
  gc <- createGC d w
  forM_ args (stablishGcProperty d gc)
  return gc
  where
    stablishGcProperty d gc proper =
      case proper of
        ("fgColor", val) ->
          hexToColor val d
          >>= setForeground d gc

hexToColor :: String -> Display -> IO Pixel
hexToColor s d =
  let colormap = defaultColormap d (defaultScreen d)
   in parseColor d colormap s
        >>= allocColor d colormap
        <&> color_pixel

interpretCommands :: Display -> Window -> [DefwToken] -> IO ()
interpretCommands d w = mapM_ interpretCommand
  where
    interpretCommand command = case command of
      DefwTitle title -> storeName d w title
      _               -> return ()

collectDrawInstructions :: [DefwToken] -> [DefwToken]
collectDrawInstructions = filter isDraw
  where
    isDraw DefwDraw{} = True
    isDraw _          = False

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
