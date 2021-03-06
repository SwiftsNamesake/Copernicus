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
--        - Performance profiling (feels chuggish sometimes, consumes a lot of CPU power)
--        - Figure out how to deal with polymorphic numerical types
--        - Polymorphic wrappers around core rendering functions
--        - This module should only deal with graphics
--        - Work out how to deal with state (logical/interaction/graphical)
--        - Wrap up graphical/animation state in single object (eg. Renderable ball with trail/colour/arrows/etc.)
--          -- Toggle visibility (eg. with checkbuttons)
--          -- Drawing bounding boxes, vector arrows
--          -- Create Drawable type
--
--        - Effects
--          -- Fading
--
--        - Worry about clean-up or rely on GC (eg. freeing surfaces) (?)
--
--        - Shape API
--          -- Should render functions simply set a path or fill them in/draw them too
--          -- Utility functions for colour/fill/stroke/line width/etc. (?)

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
import Data.IORef

import qualified Copernicus as Cop
import qualified Palette



---------------------------------------------------------------------------------------------------
-- Types
---------------------------------------------------------------------------------------------------
-- TODO: One trail per body ([[Complex Double]]) (?)
data World = World { frame :: Int, size :: (Int, Int), bodies :: [Cop.Body Double], trails :: [[Complex Double]] } deriving Show



---------------------------------------------------------------------------------------------------
-- Data
---------------------------------------------------------------------------------------------------
animate = True --
π       = pi   -- :: Double -- TODO: Polymorphic (?)
τ       = 2*π  -- :: Double -- TODO: Polymorphic (?)
fps     = 30   :: Int

v = 2.0:+0.5 --40.0 :+ 20.0
g = 0.0:+(-9.82) --0.0 :+ (-9.82)

bodies' = map (\ (p', v', g') -> Cop.Body p' v' g') [(0.0:+1.0, v, g), (1.0:+0.0, (2.0):+1.50, g), (3.0:+(2.80), v, g), (2.5:+1.92, v, g)]



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

    set window [ containerChild := frame]
    windowSetDefaultSize window 650 650

    widgetAddEvents canvas [PointerMotionMask]

    widgetShowAll window

    -- Assets
    -- planets <- C.withImageSurfaceFromPNG "assets/planets.png" $ \surface -> surface
    planets <- C.liftIO $ C.imageSurfaceCreateFromPNG "assets/planets.png"

    -- Animation
    (w,h)    <- widgetSize window
    worldVar <- newIORef $ World { frame=0, size=(w,h), bodies=bodies', trails=map (const []) bodies' } --
    when animate $ timeoutAdd (onanimate worldVar canvas) (1000 `div` fps) >> return ()


    -- Events
    canvas `on` draw $ (C.liftIO $ readIORef worldVar) >>= flip render planets -- readIORef worldVar >>= \ w -> render (fromIntegral w) (fromIntegral h) w
    canvas `on` motionNotifyEvent $ onmousemove

    window `on` configureEvent $ onresize window worldVar
    window `on` deleteEvent $ C.liftIO mainQuit >> return False -- TODO: Uhmmm... what?
    window `on` keyPressEvent $ onkeypress

    mainGUI



-- |
-- TODO: Make polymorphic
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
-- onkeypress :: 
onkeypress = do
    key <- eventKeyName
    C.liftIO $ print key
    return True



-- |
-- onmousemove :: 
onmousemove = do
    when False $ do
        (mx,my) <- eventCoordinates
        C.liftIO $ print (mx,my)
    return False



-- Animation --------------------------------------------------------------------------------------
-- |
onanimate :: IORef World -> DrawingArea -> IO Bool
onanimate world canvas = do
    -- withMVar var print
    modifyIORef world update
    widgetQueueDraw canvas
    return True



-- |
-- TODO: Refactor, make readable (State monad, lenses)
update :: World -> World
update w@(World { frame=f, bodies=bodies'' } ) = w { frame=f+1,
                                                                       bodies=map (Cop.animate (1.0/fromIntegral fps)) bodies'',
                                                                       trails=map (\ (trail, (Cop.Body p _ _)) -> take 50 $ p:trail) $ zip (trails w) bodies'' }



-- | The time elapsed (in seconds)
-- TODO: Polymorphic (?)
elapsed :: World -> Double
elapsed world = (fromIntegral $ frame world) * 1.0/fromIntegral fps



-- Rendering --------------------------------------------------------------------------------------
-- |
-- TODO: Make polymorphic
-- TODO: Apply coordinate transformations (to World perhaps; make world a monad?)
renderBody :: Cop.Body Double -> C.Render ()
renderBody (Cop.Body (x:+y) (vx:+vy) (ax:+ay)) = do
    -- C.arc (x/3 + 200) (y/3 + 200) 12 0 τ
    -- Body
    C.arc x y 12 0 τ
    C.setSourceRGBA 0.5 0.2 0.3 1.0
    C.fill

    -- Vector arrows
    C.setSourceRGBA 0.12 0.05 1 0.8
    renderArrow (x:+y) (x:+(y+vy*10)) (abs vy*10*0.8) 5 12

    C.setSourceRGBA 1 0.05 0.12 0.8
    renderArrow (x:+y) ((x+vx*10):+y) (abs vx*10*0.8) 5 12



-- |
renderWorld :: World -> C.Render ()
renderWorld world = do

    -- X-axis
    vectorise C.moveTo $ toScreenCoords world ((-5):+0)
    vectorise C.lineTo $ toScreenCoords world (  5:+0)
    C.setSourceRGBA 1 0 0 0.8
    C.stroke

    -- Y-axis
    vectorise C.moveTo $ toScreenCoords world (0:+(-5))
    vectorise C.lineTo $ toScreenCoords world (0:+  5)
    C.setSourceRGBA 0 1 0 0.8
    C.stroke

    -- Trail(s)
    -- TODO: Better way of doing 2D 'loops'
    forM_ (zip [Palette.green, Palette.red, Palette.blue] $ trails world) $ \(fill, trail) -> do
        forM_ trail $ \ dot -> do
            -- C.setSourceRGBA (cos $ 1.3*c) (sin $ 5*c) 0.2 1.0
            Palette.choose fill
            renderCircle (toScreenCoords world dot) 3

    -- Render bodies
    forM_ (bodies world) $ renderBody . bodyToScreenCoords world



-- |
render :: World -> C.Surface -> C.Render ()
render world planets = do

    --
    when False $ do 
        C.moveTo 16 44
        C.liftIO $ C.fontOptionsCreate >>= flip C.fontOptionsSetAntialias C.AntialiasSubpixel
        C.selectFontFace "Helvetica" C.FontSlantNormal C.FontWeightNormal
        C.setFontSize 30
        C.setSourceRGBA 0.62 0.62 0.62 0.7
        C.showText "Copernicus"

        extents <- C.textExtents "Copernicus"

        C.setFontSize 14
        C.setSourceRGBA 0.3 0.3 0.3 1.0
        C.moveTo 16 $ 44 + C.textExtentsHeight extents + 5
        C.showText "Jonatan H Sundqvist 2015"

    --
    renderGrid 10 10 $ fromIntegral (fst $ size world) / 10

    -- Image
    when False $ do
        {-action <- C.liftIO . C.withImageSurfaceFromPNG "assets/planets.png" $ (\surface -> return $ 
            do C.setSourceSurface surface 20 20 >> C.paint)-}
        -- planets <- C.liftIO $ C.imageSurfaceCreateFromPNG "assets/planets.png"
        C.setSourceSurface planets 50 50
        C.rectangle 320 310 250 250
        C.clip
        C.paint
        C.resetClip

    -- Arrow
    -- renderArrow (40:+40) (200:+120) 80 20 40
    -- TODO: Don't hard-code arrow values
    when False $ do
        forM_ (bodies world) $ \ body -> do
            let (from, to, len, width, headWidth) = (40:+50, let (Cop.Body p _ _) = body in toScreenCoords world p, 0.78 * magnitude (to-from), 40, 88)
            let thearrow = arrow from to len width headWidth
            renderArrow from to len width headWidth

            -- Arrow vertices
            C.setSourceRGBA 0 0 0 1.0
            forM_ thearrow (flip renderCircle 5)

            C.setSourceRGBA 0 0 0 1.0
            forM_ (zip [1..] thearrow) $ \(n, p) -> vectorise C.moveTo (p+(6:+6)) >> C.showText (show n)

            -- Arrow symmetry line
            vectorise C.moveTo from
            vectorise C.lineTo to
            C.setSourceRGBA 0 0 0 1.0
            C.stroke

    -- Shifting polygons
    when False $ do
        let sides       = 3 + (flip mod 10 . flip div fps $ frame world)
            radius mini = (+mini) . (*35.0) . (+1.0) . sin  $ elapsed world
            origin      = (w/2):+(h/2)
            fill        = False in forM_ [(10, (0.0, 0.5, 0, 1.0)),
                                          (30, (0.3, 0.0, 0.8, 1.0)),
                                          (50, (0.0, 0.7, 0.2, 1.0)),
                                          (70, (0.8, 0.8, 0.1, 1.0))] $ \(mini, colour) -> renderPolygon sides (radius mini) origin colour fill

    --  Bouncing balls
    renderWorld world

    -- Circle carousel
    when False $ do
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
renderArrow :: Complex Double -> Complex Double -> Double -> Double -> Double -> C.Render ()
renderArrow from to sl sw hw = do
    let (first:rest) = closePath $ arrow from to sl sw hw
    vectorise C.moveTo first
    forM_ rest $ vectorise C.lineTo

    -- C.setSourceRGBA 1 0.8 0.15 1.0
    C.setLineWidth 3
    C.stroke



-- | 
-- TODO: Add arguments for colour, stroke, etc.
-- TODO: Make polymorphic
renderPolygon :: Int -> Double -> Complex Double -> (Double, Double, Double, Double) -> Bool -> C.Render ()
renderPolygon sides radius origin (r,g,b,a) filled = do
    -- TODO: Refine 'wrap-around logic'
    C.moveTo fx fy
    forM_ rest $ \(x:+y) -> C.lineTo x y
    
    C.setSourceRGBA r g b a
    C.setLineWidth 12
    if filled
        then C.fill
        else C.stroke
    where ((fx:+fy):rest) = polygon sides radius origin ++ [fx:+fy]



-- | 
-- TODO: Options for fill/stroke, colour, width, etc.
renderCircle :: Complex Double -> Double -> C.Render ()
renderCircle (cx:+cy) radius = do
    C.arc cx cy radius 0 τ
    C.fill



-- |
-- TODO: Options for colour, width, closed/open, etc.
renderPath :: [Complex Double] -> C.Render ()
renderPath (p:ath) = do
    vectorise C.moveTo p
    forM_ ath $ vectorise C.lineTo
    C.stroke



-- |
-- Ugh, I hate underscores so much
-- TODO: Make polymorphic
renderCircleArc :: Int -> Complex Double -> Double -> Double -> Double -> Double -> C.Render ()
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



-- Geometry ---------------------------------------------------------------------------------------
-- |
-- TODO: Start angle
-- TODO: Invalid arguments (eg. sides < 3) (use Maybe?)
-- TODO: Make polymorphic
-- TODO: Move to geometry section
-- polygon :: (Floating f, RealFloat f) => Int -> f -> Complex f -> [Complex f]
polygon :: Int -> Double -> Complex Double -> [Complex Double] 
polygon sides radius origin = [ let θ = arg n in origin + ((radius * cos θ):+(radius * sin θ)) | n <- [1..sides]]
    where arg n = fromIntegral n * (2*π)/fromIntegral sides



-- |
-- TODO: Simplify and expound comments
arrow :: Complex Double -> Complex Double -> Double -> Double -> Double -> [Complex Double]
arrow from to sl sw hw = [from     + straight (sw/2), --
                          shaftEnd + straight (sw/2), --
                          shaftEnd + straight (hw/2), --
                          to,                         --
                          shaftEnd - straight (hw/2), --
                          shaftEnd - straight (sw/2), --
                          from     - straight (sw/2)]  --
    where along a b distance = a + (mkPolar distance . snd . polar $ b-a) -- Walk distance along the from a to b
          normal a b = let (mag, arg) = polar (b-a) in mkPolar mag (arg+π/2)
          shaftEnd = along from to sl --
          straight = along (0:+0) (normal from to) -- Vector perpendicular to the centre line
          -- shaftWidth = along (0:+0) (normal a b) (sw/2) -- Half of shaft width (vector relative to symmetry line)
          -- headWidth  = along (0:+0) (normal a b) (hw/2)




-- Utilities --------------------------------------------------------------------------------------
-- |
vectorise :: (Double -> Double -> a) -> Complex Double -> a
vectorise f (re:+im) = f re im



-- | 
-- TODO: Unsafe, use Maybe (?)
closePath :: [Complex Double] -> [Complex Double]
closePath path = path ++ [head path] 



-- Transforms a vector from one coordinate space to another
-- by applying the given scaling and translation
-- Useful for converting between simulation and screen coordinates
-- TODO: Types for representing coordinate systems
-- TODO: Use matrix instead (scale and translate should be applied in reverse order when undoing the transformation)
-- TODO: Add undo parameter (?)
-- TODO: Pure function, move to Copernicus.hs or use Cairo matrix functions
transform :: Complex Double -> Complex Double -> Complex Double -> Complex Double
transform scaling translation = scale scaling . translate translation



-- |
scale :: Complex Double -> Complex Double -> Complex Double
scale (sx:+sy) (x:+y) = (sx*x):+(sy*y) 



-- |
translate :: Complex Double -> Complex Double -> Complex Double
translate = (+)



-- |
toScreenCoords :: World -> Complex Double -> Complex Double
toScreenCoords world = transform (sx:+sy) translation
    where (w, h)   = let (w', h') = size world in (fromIntegral w', fromIntegral h')
          (sx:+sy)    = (w/10):+(-h/10)
          translation = w/(2*sx):+h/(2*sy)



-- | TODO: Implement :/
-- TODO: Dotwise application for Complex numbers
-- TODO: Don't hard-code the world-size of the canvas (currently 10-by-10)
toWorldCoords :: World -> Complex Double -> Complex Double
toWorldCoords world = translate (-translation) . scale (sx**(-1):+sy**(-1))
    where (w, h) = let (w', h') = size world in (fromIntegral w', fromIntegral h')
          (sx:+sy)    = (w/10):+(-h/10)
          translation = (w/(2*sx)):+(h/(2*sy))



-- |
bodyToScreenCoords :: World -> Cop.Body Double -> Cop.Body Double
bodyToScreenCoords world (Cop.Body p v' a') = Cop.Body (toScreenCoords world p) v' a' -- TODO: Make transformation less ad-hoc



---------------------------------------------------------------------------------------------------
-- Entry point
---------------------------------------------------------------------------------------------------
main :: IO ()
main = mainGTK