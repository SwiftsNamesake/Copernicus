-- |
-- Module      : Hieroglyphs
-- Description : Cairo experiments (graphics library)
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental
-- Portability : POSIX (not sure)
-- 
-- Jonatan H Sundqvist
-- May 31 2015
--

-- TODO | - 
--        - 


-- SPEC | -
--        -



-- module Hieroglyphs where



---------------------------------------------------------------------------------------------------
-- We'll need these
---------------------------------------------------------------------------------------------------
import Graphics.UI.Gtk -- hiding (fill)
import qualified Graphics.Rendering.Cairo as C

import Control.Monad (when)



---------------------------------------------------------------------------------------------------
-- Data
---------------------------------------------------------------------------------------------------
animate = True



---------------------------------------------------------------------------------------------------
-- Functions
---------------------------------------------------------------------------------------------------
mainGTK :: IO ()
mainGTK = do
    initGUI
    window <- windowNew

    frame <- frameNew
    canvas <- drawingAreaNew
    containerAdd frame canvas

    set window [ containerChild := frame ]
    windowSetDefaultSize window 300 300

    -- onExpose da (exposeHandler da draw)
    when animate $ timeoutAdd (widgetQueueDraw canvas >> return True) 16 >> return ()

    canvas `on` draw $ render 300 300
    window `on` deleteEvent $ C.liftIO mainQuit >> return False -- TODO: Uhmmm... what?
    
    widgetShowAll window
    mainGUI


widgetSize :: Widget -> IO (Float, Float)
widgetSize widget = do
    w <- widgetGetAllocatedWidth widget
    h <- widgetGetAllocatedHeight widget
    return (w, h)


render :: Double -> Double -> C.Render ()
render w h = do
           C.setSourceRGB 1 1 1
           C.paint

           C.setSourceRGB 0 0 0
           C.moveTo 0 0
           C.lineTo w h
           C.moveTo w 0
           C.lineTo 0 h
           C.setLineWidth (0.1 * (h + w))
           C.stroke

           C.rectangle 0 0 (0.5 * w) (0.5 * h)
           C.setSourceRGBA 1 0 0 0.8
           C.fill

           C.rectangle 0 (0.5 * h) (0.5 * w) (0.5 * h)
           C.setSourceRGBA 0 1 0 0.6
           C.fill

           C.rectangle (0.5 * w) 0 (0.5 * w) (0.5 * h)
           C.setSourceRGBA 0 0 1 0.4
           C.fill



---------------------------------------------------------------------------------------------------
-- Entry point
---------------------------------------------------------------------------------------------------
main :: IO ()
main = mainGTK