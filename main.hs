--
-- Main.hs - Copernicus
-- Entry point for the Copernicus project
--
-- Jonatan H Sundqvist
-- November 25 2014
--

-- TODO | - Model planetary motion and physics in 2D
--        - Cartoon earth with gravitational field
--        - Flexible event handling
--        - App typeclass (run, manage events, window properties, etc.)

-- SPEC | -
--        -



module Main where



---------------------------------------------------------------------------------------------------
-- We'll need these
---------------------------------------------------------------------------------------------------
import Graphics.Gloss.Data.Picture (line)
import Graphics.Gloss (circleSolid, rectangleSolid)
import Graphics.Gloss.Geometry.Angle (degToRad, radToDeg, normaliseAngle)
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Vector
import Data.Complex



---------------------------------------------------------------------------------------------------
-- Definitions
---------------------------------------------------------------------------------------------------
clampAngle :: Float -> Float
clampAngle = radToDeg . normaliseAngle . degToRad


--
parabola :: Float -> Vector -> Vector -> Vector -> Vector
parabola t p v a = (px + vx*t + 0.5*ax*t**2, py + vy*t + 0.5*ay*t**2)
	where
		(px,py) = p
		(vx,vy) = v
		(ax,ay) = a


-- Make typeclass (mesh, body, bounding box, collision, etc) (?)
data Body = Body Vector Vector Vector


--
animate :: Float -> Body -> Body
animate t (Body p v a) = collide (30+30/2-540/2) $ Body (parabola t p v a) (v + mulSV t a) a


-- collide
-- Very primitive for now
collide :: Float -> Body -> Body
collide gnd (Body (px, py) (vx, vy) a) = Body (px, py) (invertIf (\ v -> (px <= 15-720/2) || ( px >= (720/2-30/2))) vx, invertIf (\ v -> (v < 0) && (py <= gnd)) vy) a
	where invertIf p v
		| p v 		= -v
		| otherwise =  v



-- Transforms a vector from one coordinate space to another
-- by applying the given scaling and translation
-- Useful for converting between simulation and screen coordinates
transform :: Vector -> Vector -> Picture -> Picture
transform sc tr = uncurry scale sc . uncurry translate tr



---------------------------------------------------------------------------------------------------
-- Interaction
---------------------------------------------------------------------------------------------------
simulate :: IO ()
simulate = playIO
	display 	-- Window mode
	white 		-- Background colour
	60			-- FPS (simulation steps per second, technically)
	world 		-- Initial world
	render		-- Converts world to Picture
	respond 	-- User interaction
	advance 	-- Advances the world to the next simulation step
	where
		display  			  = InWindow "Simulator" (width, height) (25, 25)
		world 	 			  = map (\ (p, v, g) -> Body p v g) [((0.0,0.0), v, g), ((10.0,0.0), (-20.0, 15.0), g), ((35.0,-28.0), v, g)]
		render w 	 		  = return . pictures $ (map (uncurry drawBall) $ zip colours w) ++ [drawGround w, renderGrid 15 15 width height] -- TODO: Refactor this ugly mess
		drawBall col (Body (x, y) _ _)  = color col . translate x y $ circleSolid 15 -- TODO: Reorder arguments (?)
		drawGround _ 		  = translate 0 (30/2-fromIntegral height/2) . color green $ rectangleSolid (fromIntegral width) 30
		respond e w 		  = return w
		advance t w 		  = return . map (animate t) $ w
		v 					  = (40.0, 20.0) --40.0 :+ 20.0
		g 					  = (0.0, -98.2) --0.0 :+ (-9.82)
		(width, height) 	  = (740, 540)
		colours = cycle [red, green, orange]
		--parabola t v a p = let (px:+py) = p; (vx:+vy) = v; (ax:+ay) = a in (px + vx*t + 0.5*ax*t**2):+(py + vy*t + 0.5*ay*t**2)



--
--renderForces


--
-- Subgrids, colours, thickness, markings, hover for coordinates, snap-to-grid
renderGrid :: Float -> Float -> Int -> Int -> Picture
renderGrid dx dy w' h' = pictures $ rows ++ cols
	where
		cols = map (\cl -> line [(cl*dx-w/2, -h/2), (cl*dx-w/2, h/2)]) [1..cls]
		rows = map (\rw -> line [(-w/2, rw*dy-h/2), (w/2, rw*dy-h/2)]) [1..rws]
		rws  = h / dy -- Number of rows
		cls  = w / dx -- Number of columns
		w 	 = fromIntegral w'
		h 	 = fromIntegral h'



---------------------------------------------------------------------------------------------------
-- Entry point
---------------------------------------------------------------------------------------------------
main :: IO ()
main = do
	simulate