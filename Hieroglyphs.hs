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
data World = World { frame :: Int, size :: (Int, Int), bodies :: [Cop.Body] } deriving Show



---------------------------------------------------------------------------------------------------
-- Data
---------------------------------------------------------------------------------------------------
animate = True :: Bool   --
π       = pi   -- :: Double -- TODO: Polymorphic (?)
τ       = 2*π  -- :: Double -- TODO: Polymorphic (?)
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
    windowSetDefaultSize window 650 650

    widgetShowAll window

    -- Assets
    -- planets <- C.withImageSurfaceFromPNG "assets/planets.png" $ \surface -> surface

    -- Animation
    (w,h)    <- widgetSize window
    worldVar <- newIORef $ World { frame=0, size=(w,h), bodies=bodies' } --
    when animate $ timeoutAdd (onanimate worldVar canvas) (1000 `div` fps) >> return ()

    -- Events
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



-- Animation --------------------------------------------------------------------------------------
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



-- | The time elapsed (in seconds)
-- TODO: Polymorphic (?)
elapsed :: World -> Double
elapsed world = (fromIntegral $ frame world) * 1.0/fromIntegral fps



-- Rendering --------------------------------------------------------------------------------------
-- |
renderBody :: Cop.Body -> C.Render ()
renderBody (Cop.Body (x:+y) v' g') = do
    C.arc (x/3 + 200) (y/3 + 200) 12 0 τ
    C.setSourceRGBA 0 0.2 0.3 1.0
    C.fill



-- |
renderWorld :: World -> C.Render ()
renderWorld world = forM_ (bodies world) renderBody



-- |
render :: World -> C.Render ()
render world = do
    renderGrid 10 10 $ fromIntegral (fst $ size world) / 10

    C.moveTo 30 30
    C.liftIO $ C.fontOptionsCreate >>= flip C.fontOptionsSetAntialias C.AntialiasSubpixel
    C.selectFontFace "Helvetica" C.FontSlantNormal C.FontWeightNormal
    C.setFontSize 30
    C.setSourceRGBA 0.62 0.62 0.62 0.7
    C.showText "Copernicus"
    
    let sides       = 3 + (flip mod 10 . flip div fps $ frame world)
        radius mini = (+mini) . (*35.0) . (+1.0) . sin  $ elapsed world
        origin      = (w/2):+(h/2)
        fill        = False in do
        renderPolygon sides (radius 10) origin (0,   0.5, 0,   1.0) fill
        renderPolygon sides (radius 25) origin (0.3, 0.0, 0.8, 1.0) fill
        renderPolygon sides (radius 40) origin (0.0, 0.7, 0.2, 1.0) fill

    renderWorld world
    renderCircleArc 10 origin spread radius begin τ
    where count  = 10
          origin = (w/2):+(h/2) 
          spread = 50 + 50 * (1 + sin (τ * rpm * elapsed world)) -- 134      -- Radius of the big circle (pixels?)
          radius = 20       -- Radius of a small circle (pixels?)
          (w,h)  = let (w', h') = size world in (fromIntegral w', fromIntegral h')
          rpm    = 0.3      --
          begin  = τ * rpm * elapsed world



-- |
renderGrid :: Int -> Int -> Double -> C.Render ()
renderGrid cols rows size = do
    sequence_ [ tilePath cl rw >> C.fill   | cl <- [0..(cols-1)], rw <- [0..(rows-1)] ] -- Tiles
    sequence_ [ tilePath cl rw >> C.stroke | cl <- [0..(cols-1)], rw <- [0..(rows-1)] ] -- Borders
    -- TODO: Figure out how to use fill AND stroke
    where chooseColour cl rw = if (cl `mod` 2) == (rw `mod` 2) then 0.3 else 0.75 -- TODO: This should be a utility function
          tilePath cl rw     = C.rectangle (fromIntegral cl*size) (fromIntegral rw*size) size size >> C.setSourceRGBA 0.22 0.81 (chooseColour cl rw) 0.32



-- |
-- TODO: Start angle
-- TODO: Invalid arguments (eg. sides < 3) (use Maybe?)
polygon :: Floating f => Int -> f -> Complex f -> [Complex f]
polygon sides radius origin = [ let θ = arg n in origin + ((radius * cos θ):+(radius * sin θ)) | n <- [1..sides]]
    where arg n = (fromIntegral n * (2*π)/fromIntegral sides :: f)



-- | 
-- TODO: Add arguments for colour, stroke, etc.
renderPolygon :: Floating f => Int -> f -> Complex f -> (Double, Double, Double, Double) -> Bool -> C.Render ()
renderPolygon sides radius origin (r,g,b,a) filled = do
    -- TODO: Refine 'wrap-around logic'
    let ((fx:+fy):rest) = take (sides + 1) . cycle $ polygon sides radius origin in C.moveTo fx fy >> forM_ rest (\(x:+y) -> C.lineTo x y)
    C.setSourceRGBA r g b a
    C.setLineWidth 12
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
renderCircleArc :: Floating f => Int -> Complex f -> f -> f -> f -> f -> C.Render ()
renderCircleArc
    count    -- Number of small circles
    (ox:+oy) -- Centre of the 'arc' (pixels?)
    spread   -- Radius of the 'arc'
    radius   -- Radius of the small circles
    begin    -- Start angle of the arc
    extent = forM_ [1..count] $ \ n -> do
        let n' = fromIntegral n
        let θ  = begin + n'*extent/fromIntegral count
        C.arc (ox - spread*cos θ) (oy - spread*sin θ) radius 0 τ
        C.setSourceRGBA (0.5 * (1 + sin θ)) (0.1*n') (1/n') 0.95
        C.fill



---------------------------------------------------------------------------------------------------
-- Entry point
---------------------------------------------------------------------------------------------------
main :: IO ()
main = mainGTK