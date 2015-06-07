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

-- TODO | - Lenses
--        - Performance profiling (feels chuggish)


-- SPEC | -
--        -



-- module Hieroglyphs where



---------------------------------------------------------------------------------------------------
-- We'll need these
---------------------------------------------------------------------------------------------------
import Graphics.UI.Gtk -- hiding (fill)
import qualified Graphics.Rendering.Cairo as C

import Data.Complex
import Control.Monad (when, forM_)
-- import Control.Concurrent.MVar
import Data.IORef

import qualified Copernicus as Cop



---------------------------------------------------------------------------------------------------
-- Types
---------------------------------------------------------------------------------------------------
data World = World { frame :: Integer, size :: (Int, Int), bodies :: [Cop.Body] } deriving Show



---------------------------------------------------------------------------------------------------
-- Data
---------------------------------------------------------------------------------------------------
animate = True :: Bool   --
π       = pi   :: Double -- TODO: Polymorphic (?)
fps     = 30   :: Int

-- g = 0:+9.82 -- TODO: Negate
-- v = 18:+5   -- TODO:
v = 40.0:+20.0 --40.0 :+ 20.0
g = 0.0:+(-98.2) --0.0 :+ (-9.82)

bodies' = map (\ (p', v', g') -> Cop.Body p' v' g') [(0.0:+0.0, v, g), (10.0:+0.0, (-20.0):+15.0, g), (35.0:+(-28.0), v, g), (20.5:+19.2, v, g)]



---------------------------------------------------------------------------------------------------
-- Functions
---------------------------------------------------------------------------------------------------
-- |
mainGTK :: IO ()
mainGTK = do
    initGUI
    window <- windowNew

    frame  <- frameNew
    canvas <- drawingAreaNew
    containerAdd frame canvas

    set window [ containerChild := frame ]
    windowSetDefaultSize window 500 500

    widgetShowAll window

    (w,h)    <- widgetSize window
    worldVar <- newIORef $ World { frame=0, size=(w,h), bodies=bodies' } --
    when animate $ timeoutAdd (onanimate worldVar canvas) (1000 `div` fps) >> return ()

    canvas `on` draw $ (C.liftIO $ readIORef worldVar) >>= render -- readIORef worldVar >>= \ w -> render (fromIntegral w) (fromIntegral h) w
    window `on` configureEvent $ onresize window worldVar
    window `on` deleteEvent $ C.liftIO mainQuit >> return False -- TODO: Uhmmm... what?

    mainGUI



-- |
widgetSize :: WidgetClass self => self -> IO (Int, Int)
widgetSize widget = do
    w <- widgetGetAllocatedWidth widget
    h <- widgetGetAllocatedHeight widget
    return (w, h)



-- |
-- onresize :: WidgetClass self => self -> IORef World -> IO Bool
onresize window worldVar = do
    sz <- eventSize
    C.liftIO $ modifyIORef worldVar $ \ wd -> wd { size=sz }
    return True



-- |
onanimate :: IORef World -> DrawingArea -> IO Bool
onanimate world canvas = do
    -- withMVar var print
    modifyIORef world update
    widgetQueueDraw canvas
    return True



-- |
update :: World -> World
update w@(World { frame=f, bodies=b } ) = w { frame=f+1, bodies=map (Cop.animate (1.0/fromIntegral fps)) b }



-- |
renderBody :: Cop.Body -> C.Render ()
renderBody (Cop.Body (x:+y) v' g') = do
    C.arc (realToFrac x/3 + 200) (realToFrac y/3 + 200) 12 0 (2*π)
    C.setSourceRGBA 0 0.2 0.3 1.0
    C.fill



-- |
renderWorld :: World -> C.Render ()
renderWorld world = forM_ (bodies world) renderBody



-- |
render :: World -> C.Render ()
render world = do
    renderWorld world
    renderCircleArc 10 origin spread radius begin (2*π)
    renderGrid 10 10 68
    where count  = 10
          origin = (w/2):+(h/2) 
          spread = 50 + 50 * (1 + sin (2.0*π * rpm * fromIntegral (frame world) * 1.0 / fromIntegral fps)) -- 134      -- Radius of the big circle (pixels?)
          radius = 20       -- Radius of a small circle (pixels?)
          (w,h)  = let (w', h') = size world in (fromIntegral w', fromIntegral h')
          rpm    = 0.3      --
          begin  = 2.0*π * rpm * fromIntegral (frame world) * (1.0 / fromIntegral fps)
          -- begin  = 2.0*π*(fromIntegral n / fromIntegral count) + 2.0*π * rpm * fromIntegral frm * (1.0 / fromIntegral fps) -- in



renderGrid :: Int -> Int -> Double -> C.Render ()
renderGrid cols rows size = do
    sequence_ [ renderTile (fromIntegral cl) (fromIntegral rw) | cl <- [1..cols], rw <- [1..rows] ]
    where chooseColour cl rw = if (cl `mod` 2) == (rw `mod` 2) then 0.3 else 0.75 -- TODO: This should be a utility function
          renderTile cl rw   = C.rectangle (cl*size) (rw*size) size size >> C.setSourceRGBA 0.22 0.81 (chooseColour (floor cl) (floor rw)) 0.32 >> C.fill



polygon :: RealFloat f => Int -> f -> Complex f -> [Complex f]
polygon sides radius origin = [ let θ = fromIntegral n * 2*pi/fromIntegral sides in origin + ((radius * cos θ):+(radius * sin θ)) | n <- [1..sides]]



-- | 
renderPolygon :: Int -> Double -> Cop.Vector -> Bool -> C.Render ()
renderPolygon sides radius origin filled = do
    let (sx:+sy) = origin+(realToFrac radius:+0) in C.moveTo (realToFrac sx) (realToFrac sy)
    forM_ [1..sides] $ \n -> let θ        = fromIntegral n * π*2/fromIntegral sides
                                 (vx:+vy) = origin+((realToFrac radius * cos θ) :+ (realToFrac radius * sin θ))
                             in C.lineTo vx vy
    C.setSourceRGBA 0 0 0 1.0
    if filled
        then C.fill
        else C.stroke
    -- when filled C.fill


-- |
renderCross :: Double -> Double -> C.Render ()
renderCross w h = do
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



-- |
-- Ugh, I hate underscores so much
renderCircleArc :: Int -> Cop.Vector -> Double -> Double -> Double -> Double -> C.Render ()
renderCircleArc
    count    -- Number of small circles
    (ox:+oy) -- Centre of the 'arc' (pixels?)
    spread   -- Radius of the 'arc'
    radius   -- Radius of the small circles
    begin    -- Start angle of the arc
    extent = forM_ [1..count] $ \ n -> do
        let n' = fromIntegral n
        let θ  = begin + n'*extent/fromIntegral count
        C.arc (realToFrac ox - spread*cos θ) (realToFrac oy - spread*sin θ) radius 0 (2*π)
        C.setSourceRGBA (0.5 * (1 + sin θ)) (0.1*n') (1/n') 0.95
        C.fill



---------------------------------------------------------------------------------------------------
-- Entry point
---------------------------------------------------------------------------------------------------
main :: IO ()
main = mainGTK