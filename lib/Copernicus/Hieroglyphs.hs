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
--
--        - Application states

-- SPEC | -
--        -



---------------------------------------------------------------------------------------------------
-- Compiler instructions
---------------------------------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}




---------------------------------------------------------------------------------------------------
-- API definition
---------------------------------------------------------------------------------------------------
-- Including a module statement seems to interfer with the compilation process. I need to figure
-- out how to easily toggle between compile-as-module and compile-as-main.
module Copernicus.Hieroglyphs where



---------------------------------------------------------------------------------------------------
-- We'll need these
---------------------------------------------------------------------------------------------------
import Graphics.UI.Gtk                         --
import qualified Graphics.Rendering.Cairo as C --

import Data.Complex                --
import Control.Monad (when, forM_) --
import Data.IORef                  --
import Text.Printf                 --

-- import qualified Control.Lens (makeLenses) --
import Control.Monad.State (State, execState, get)

-- TODO: Collect my libraries in Southpaw package
import Utilities.Utilities (chunks)

import qualified Copernicus.Core as Cop --
import qualified Palette           --



---------------------------------------------------------------------------------------------------
-- Types
---------------------------------------------------------------------------------------------------
-- |
-- TODO: One trail per body ([[Complex Double]]) (?)
-- TODO: Connect bodies and trails (tie related data together properly, eg. with bundled updates like addBody)
data World = World { frame   :: Int,                -- Frame number
                     playing :: Bool,               -- Simulation and animations are playing
                     size    :: (Int, Int),         -- Window size
                     bodies  :: [Core.Body Double],  -- Bouncing balls
                     clicks  :: [Complex Double],   -- Click positions
                     trails  :: [[Complex Double]], -- Past body positions
                     transaction :: Transaction     --
                   } deriving Show


-- |
data Settings = Settings { 
    
                         }


-- |
-- TODO: Additional data (eg. parameterised on some extra data bundle type)
data Drawable = Drawable { visible :: Bool,                         --
                           centre  :: Complex Double,               --
                           paint   :: Complex Double -> C.Render () --
                         } --deriving Show


-- |
data Transaction = Transaction (Maybe (Complex Double)) (Maybe (Complex Double)) deriving Show



---------------------------------------------------------------------------------------------------
-- Data
---------------------------------------------------------------------------------------------------
animate = True --
π       = pi   -- :: Double -- TODO: Polymorphic (?)
τ       = 2*π  -- :: Double -- TODO: Polymorphic (?)
fps     = 30   :: Int

v = 2.0:+0.5 --40.0 :+ 20.0
g = 0.0:+(-9.82) --0.0 :+ (-9.82)



---------------------------------------------------------------------------------------------------
-- Lenses
---------------------------------------------------------------------------------------------------
-- Control.Lens.makeLenses ''World
-- Lens.makeLenses ''Core.Body

sizeAsVector :: World -> Complex Double
sizeAsVector world = let (x, y) = size world in (fromIntegral x):+(fromIntegral y)



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

    widgetAddEvents canvas [PointerMotionMask] -- MouseButton1Mask

    widgetShowAll window

    -- Assets
    -- planets <- C.withImageSurfaceFromPNG "assets/planets.png" $ \surface -> surface
    planets <- C.liftIO $ C.imageSurfaceCreateFromPNG "assets/planets.png"

    -- Animation
    (w,h)    <- widgetSize window
    worldVar <- newIORef $ createWorld w h --
    when animate $ timeoutAdd (onanimate worldVar canvas) (1000 `div` fps) >> return ()

    -- Events
    canvas `on` draw $ ondraw worldVar planets
    canvas `on` motionNotifyEvent $ onmousemove worldVar

    canvas `on` buttonPressEvent   $ onbuttonpress worldVar
    canvas `on` buttonReleaseEvent $ onbuttonreleased worldVar

    window `on` configureEvent $ onresize window worldVar
    window `on` deleteEvent $ C.liftIO mainQuit >> return False -- TODO: Uhmmm... what?
    window `on` keyPressEvent $ onkeypress

    mainGUI



-- Event helpers ----------------------------------------------------------------------------------
-- |
-- TODO: Make polymorphic
widgetSize :: WidgetClass self => self -> IO (Int, Int)
widgetSize widget = do
    w <- widgetGetAllocatedWidth widget
    h <- widgetGetAllocatedHeight widget
    return (w, h)



-- Events -----------------------------------------------------------------------------------------
-- |
-- onresize :: WidgetClass self => self -> IORef World -> IO Bool
onresize window worldVar = do
    sz <- eventSize
    C.liftIO $ modifyIORef worldVar $ \ wd -> wd { size=sz }
    return True



-- |
-- onkeypress :: IORef World -> IO Bool
onkeypress = do
    key <- eventKeyName
    C.liftIO $ print key
    return True



-- |
-- onbuttonpress :: IORef World -> IO Bool
onbuttonpress worldVar = do
    (x, y) <- eventCoordinates
    world <- C.liftIO $ readIORef worldVar
    C.liftIO $ printf "Added body\n"
    C.liftIO $ modifyIORef worldVar $ \ wd@(World { clicks=c, trails=t }) -> wd { clicks=(x:+y):c,
                                                                                  trails=[]:t,
                                                                                  transaction=let p = Just . toWorldCoords world $ x:+y in Transaction p p }
    return True



-- |
-- onbuttonreleased ::
onbuttonreleased worldVar = do
        (x, y) <- eventCoordinates
        C.liftIO $ modifyIORef worldVar $ \ wd@(World { transaction=Transaction (Just fr) (Just to), bodies=b, trails=t }) -> wd { transaction=Transaction Nothing Nothing,
                                                                                                                         bodies=Core.Body fr ((3:+0) * (fr-to)) g : b,
                                                                                                                         trails=[]:t }
        return True



-- |
-- onmousemove :: 
onmousemove worldVar = do
    (mx,my) <- eventCoordinates
    C.liftIO $ modifyIORef worldVar $ \ wd@(World { transaction=Transaction a b }) -> case (a, b) of
        (Just from, Just to) -> wd { transaction=Transaction a (Just . toWorldCoords wd $ mx:+my) }
        _                    -> wd
    return False



-- | 
-- ondraw ::
ondraw worldVar planets = do
    world <- C.liftIO  $ readIORef worldVar
    render world planets -- readIORef worldVar >>= \ w -> render (fromIntegral w) (fromIntegral h) w



-- Animation --------------------------------------------------------------------------------------
-- | In the beginning...
createWorld :: Int -> Int -> World
createWorld w h = World { frame=0,
                          playing=True,
                          size=(w,h),
                          bodies=[], --bodies',
                          trails=map (const []) bodies',
                          clicks=[],
                          transaction=Transaction Nothing Nothing }
    where bodies' = map (\ (p', v') -> Core.Body p' v' g) [(0.0:+1.0, v), (1.0:+0.0, (2.0):+1.50), (3.0:+(2.80), v), (2.5:+1.92, v), (3.5:+1.92, 2.2:+(-3.5)), (4.5:+0.92, v)]



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
                                                     bodies=map (Core.animate (1.0/fromIntegral fps)) bodies'',
                                                     trails=map (\ (trail, body) -> take 50 $ Core.position body:trail) $ zip (trails w) bodies'' }



-- | The time elapsed (in seconds)
-- TODO: Polymorphic (?)
elapsed :: World -> Double
elapsed world = (fromIntegral $ frame world) * 1.0/fromIntegral fps



-- Rendering --------------------------------------------------------------------------------------
-- |
-- TODO: Make polymorphic
-- TODO: Apply coordinate transformations (to World perhaps; make world a monad?)
renderBody :: Core.Body Double -> World -> C.Render ()
renderBody (Core.Body p (vx:+vy) (ax:+ay)) world = do
    -- Body
    vectorise C.arc (toScreenCoords world p) 12 0 τ
    C.setSourceRGBA 0.5 0.2 0.3 1.0
    C.fill

    -- Vector arrows
    -- TODO: Scaling and labelling vectors
    renderVector (0:+0.2*vy) (0.12, 0.05, 1, 0.8)
    renderVector (0.2*vx:+0) (1, 0.05, 0.12, 0.8)
    where renderVector apart cl = do
            let from = toScreenCoords world p
            let to   = toScreenCoords world $ p + apart
            renderArrow from to ((0.8 *) . magnitude $ to - from) 5 12
            Palette.choose cl
            C.fill



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
    forM_ (zip (cycle colours) . map (map $ toScreenCoords world) $ trails world) $ uncurry renderTrail

    -- Render temporary ball and arrow
    case transaction world of
        (Transaction (Just from) (Just to)) -> do
            let (from', to') = (toScreenCoords world from, toScreenCoords world to)
            -- C.liftIO $ putStrLn "Testing 1"
            -- C.liftIO $ printf "from=(%f, %f), to=(%f, %f)" (realPart from') (imagPart from') (realPart to') (imagPart to')
            renderCircle from' 12

            Palette.choose Palette.turquoise
            renderArrow from' (from' + (from'-to')) ((0.8 *) . magnitude $ to'-from') 14 24 
            C.fill
        _                                   -> return ()

    -- Render bodies
    forM_ (bodies world) $ flip renderBody world
    where colours = [Palette.aquamarine,
                     Palette.gold,
                     Palette.orange,
                     Palette.crimson,
                     Palette.firebrick,
                     Palette.darkviolet,
                     Palette.darkseagreen,
                     Palette.hotpink,
                     Palette.honeydew,
                     Palette.azure]


-- | renderTrail
renderTrail :: Palette.Colour -> [Complex Double] -> C.Render ()
renderTrail fill trail = forM_ trail $ \dot -> do
    Palette.choose fill
    renderCircle dot 3



-- |
renderGrid :: Int -> Int -> Double -> C.Render ()
renderGrid cols rows size = do
    -- TODO: Figure out how to use fill AND stroke
    C.setLineWidth 4
    gridM_ cols rows $ \ cl rw -> tilePath cl rw >> C.fill   --
    gridM_ cols rows $ \ cl rw -> tilePath cl rw >> C.stroke --
    where chooseColour cl rw = if (cl `mod` 2) == (rw `mod` 2) then 0.3 else 0.75 -- TODO: This should be a utility function
          tilePath cl rw     = C.rectangle (fromIntegral cl*size) (fromIntegral rw*size) size size >> C.setSourceRGBA 0.22 0.81 (chooseColour cl rw) 0.32



-- |
renderArrow :: Complex Double -> Complex Double -> Double -> Double -> Double -> C.Render ()
renderArrow from to sl sw hw = do
    let (first:rest) = closePath $ arrow from to sl sw hw
    vectorise C.moveTo first
    forM_ rest $ vectorise C.lineTo



-- | 
-- TODO: Add arguments for colour, stroke, etc.
-- TODO: Make polymorphic
renderPolygon :: Integral int => int -> Double -> Complex Double -> (Double, Double, Double, Double) -> Bool -> C.Render ()
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
renderPath []      = return ()
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



-- |
renderBezier :: Complex Double -> Complex Double -> Complex Double -> C.Render ()
renderBezier (x1:+y1) (x2:+y2) (x3:+y3) = C.curveTo x1 y1 x2 y2 x3 y3



-- Tailored render functions ----------------------------------------------------------------------
-- | Renders onto the canvas on each update
-- TODO: Rename
render :: World -> C.Surface -> C.Render ()
render world planets = do
    when True  $ renderSigns                                             --
    when True  $ renderGrid 10 10 $ fromIntegral (fst $ size world) / 10 --
    when False $ renderPlanets planets                                   -- Image

    when False . renderWonkyArrowsPointingAtBalls . map (toScreenCoords world . Core.position) $ bodies world -- Arrow
    when False $ renderNestedPolygons ((0.5:+0)*sizeAsVector world) (elapsed world)                          -- Shifting polygons

    when True  $ renderWorld world                                                  -- Bouncing balls
    when False $ renderCircleCarousel ((0.5:+0)*sizeAsVector world) (elapsed world) -- Circle carousel

    when False $ renderClicks (take 20 $ clicks world) -- 
    when False $ renderCurvedPath $ clicks world       -- 



-- |
renderSigns :: C.Render ()
renderSigns = do
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



-- |
renderPlanets :: C.Surface -> C.Render ()
renderPlanets planets = do
    C.setSourceSurface planets 50 50
    C.rectangle 320 310 250 250
    C.clip
    C.paint
    C.resetClip



-- |
renderWonkyArrowsPointingAtBalls :: [Complex Double] -> C.Render ()
renderWonkyArrowsPointingAtBalls targets = do
    forM_ targets $ \ target -> do
        let (from, to, len, width, headWidth) = (40:+50, target, 0.78 * magnitude (to-from), 40, 88)
        let thearrow = arrow from to len width headWidth
        
        renderArrow from to len width headWidth
        C.setLineWidth 3
        C.stroke

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



-- |
renderNestedPolygons :: Complex Double -> Double -> C.Render ()
renderNestedPolygons origin time = forM_ (zip [10, 30..] $ cycle colours) $ \(mini, colour) -> renderPolygon sides (radius mini) origin colour fill
    where sides       = 3 + (floor time `mod` 10)
          radius mini = (+mini) . (*35.0) . (+1.0) . sin $ time
          fill        = False
          colours     = [ Palette.cornflowerblue,
                          Palette.darkolivegreen,
                          Palette.darkorchid,
                          Palette.darkslategray,
                          Palette.crimson,
                          Palette.darkgoldenrod,
                          Palette.darkseagreen,
                          Palette.hotpink,
                          Palette.honeydew]



-- |
renderCircleCarousel :: Complex Double -> Double -> C.Render ()
renderCircleCarousel origin time = renderCircleArc 10 origin spread radius begin τ
    where count  = 10
          spread = 50 + 50 * (1 + sin (τ * rpm * time)) -- 134      -- Radius of the big circle (pixels?)
          radius = 20       -- Radius of a small circle (pixels?)
          rpm    = 0.3      --
          begin  = τ * rpm * time




-- |
renderClicks :: [Complex Double] -> C.Render ()
renderClicks clicks' = do
    Palette.choose Palette.darkmagenta
    renderPath clicks'



-- |
renderCurvedPath :: [Complex Double] -> C.Render ()
renderCurvedPath path = do
    forM_ (chunks 3 path) $ \chunk -> case chunk of
        [a,b,c] -> renderBezier a b c
        _       -> return ()
    C.setLineWidth 12
    Palette.choose Palette.darkturquoise
    C.stroke



-- Geometry ---------------------------------------------------------------------------------------
-- |
-- TODO: Start angle
-- TODO: Invalid arguments (eg. sides < 3) (use Maybe?)
-- TODO: Make polymorphic
-- TODO: Move to geometry section
-- polygon :: (Floating f, RealFloat f) => Int -> f -> Complex f -> [Complex f]
polygon :: Integral int => int -> Double -> Complex Double -> [Complex Double]
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
                          from     - straight (sw/2)] --
    where along a b distance = (a +) . mkPolar distance . snd . polar $ b-a -- Walk distance along the from a to b
          normal a b = let (mag, arg) = polar (b-a) in mkPolar mag (arg+π/2)
          shaftEnd = along from to sl --
          straight = along (0:+0) (normal from to) -- Vector perpendicular to the centre line




-- Utilities --------------------------------------------------------------------------------------
-- |
vectorise :: (Double -> Double -> a) -> Complex Double -> a
vectorise f (re:+im) = f re im



-- | 
-- TODO: Unsafe, use Maybe (?)
closePath :: [Complex Double] -> [Complex Double]
closePath path = path ++ [head path]



-- |
grid :: (Integral n, Enum n) => n -> n -> (n -> n -> a) -> [a] 
grid cols rows f = [ f cl rw | cl <- [0..(cols-1)], rw <- [0..(rows-1)] ] -- Tiles



-- | 
-- TODO: Rename (eg. something pertaining to 2D loops)
-- TODO: Underscores, grrr
gridM_ :: (Integral n, Enum n, Monad m) => n -> n -> (n -> n -> m a) -> m ()
gridM_ cols rows f = sequence_ $ grid cols rows f



-- Transforms a vector from one coordinate space to another
-- by applying the given scaling and translation
-- Useful for converting between simulation and screen coordinates
-- TODO: Types for representing coordinate systems
-- TODO: Use matrix instead (scale and translate should be applied in reverse order when undoing the transformation)
-- TODO: Add undo parameter (?)
-- TODO: Pure function, move to Copernicus.hs or use Cairo matrix functions
-- transform :: Complex Double -> Complex Double -> Complex Double -> Complex Double
-- transform scaling translation = scale scaling . translate translation



-- Coordinate systems -----------------------------------------------------------------------------
-- |
scale :: Complex Double -> Complex Double -> Complex Double
scale (sx:+sy) (x:+y) = (sx*x):+(sy*y) 



-- |
translate :: Complex Double -> Complex Double -> Complex Double
translate = (+)



-- |
toScreenCoords :: World -> Complex Double -> Complex Double
toScreenCoords world = scale (sx:+sy) . translate translation
    where (w, h)   = let (w', h') = size world in (fromIntegral w', fromIntegral h')
          (sx:+sy)    = (w/10):+(-h/10)
          translation = w/(2*sx):+h/(2*sy)



-- | TODO: Implement :/
-- TODO: Dotwise application for Complex numbers
-- TODO: Don't hard-code the world-size of the canvas (currently 10-by-10)
toWorldCoords :: World -> Complex Double -> Complex Double
toWorldCoords world = translate (-translation) . scale (sx**(-1):+sy**(-1))
    where (w:+h)      = sizeAsVector world
          (sx:+sy)    = (w/10):+(-h/10)
          translation = (w/(2*sx)):+(h/(2*sy))



-- |
bodyToScreenCoords :: World -> Core.Body Double -> Core.Body Double
bodyToScreenCoords world (Core.Body p v' a') = Core.Body (toScreenCoords world p) v' a' -- TODO: Make transformation less ad-hoc



---------------------------------------------------------------------------------------------------
-- Entry point
---------------------------------------------------------------------------------------------------
main :: IO ()
main = mainGTK