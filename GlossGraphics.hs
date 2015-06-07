-- |
-- Module      : main
-- Description : Graphics module leveraging the Gloss package
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental
-- Portability : POSIX (not sure)
-- 
-- Jonatan H Sundqvist
-- June 2 2015
--

-- TODO | - 
--        - 

-- SPEC | -
--        -



module GlossGraphics where



---------------------------------------------------------------------------------------------------
-- We'll need these
---------------------------------------------------------------------------------------------------
-- import Graphics.Gloss.Data.Picture (line)
import qualified Graphics.Gloss as Gloss --(circleSolid, rectangleSolid)
import Graphics.Gloss.Interface.IO.Game hiding (Vector)
-- import Graphics.Gloss.Geometry.Angle (degToRad, radToDeg, normaliseAngle)
-- import Graphics.Gloss.Data.Vector

import Data.Complex

import Copernicus



---------------------------------------------------------------------------------------------------
-- Types
---------------------------------------------------------------------------------------------------
data World = World { bodies :: [Body],
                     grid   :: Bool
                   }



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
		display = InWindow "Simulator" (width, height) (25, 25)
		world   = World { bodies = map (\ (p', v', g') -> Body p' v' g') [(0.0:+0.0, v, g), (10.0:+0.0, (-20.0):+15.0, g), (35.0:+(-28.0), v, g), (20.5:+19.2, v, g)],
	                      grid   = True
	              }
		-- TODO: Refactor this ugly mess
		render w 	 		  = return . pictures $ zipWith drawBall colours (bodies w) ++ [drawGround (bodies w)] ++ if grid w then [renderGrid 45 45 width height] else [] 
		drawBall col (Body (x:+y) _ _) = color col . translate x y $ circleSolid 15 												-- TODO: Reorder arguments (?)
		drawGround _ 		  = translate 0 (30/2-fromIntegral height/2) . color green $ Gloss.rectangleSolid (fromIntegral width) 30
		respond e w           = return $ case e of
			EventKey (Char 'g') Down _ _ -> w { grid = not $ grid w }
			_                            -> w

		advance t w 		  = return $ w { bodies=map (animate t) $ bodies w }
		v 					  = 40.0:+20.0 --40.0 :+ 20.0
		g 					  = 0.0:+(-98.2) --0.0 :+ (-9.82)
		(width, height) 	  = (740, 540)
		colours = cycle [red, green, orange, makeColor 0.2 0.1 0.3 1.0]
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



-- Transforms a vector from one coordinate space to another
-- by applying the given scaling and translation
-- Useful for converting between simulation and screen coordinates
-- TODO: Make pure (eg. use for pure coordinates so it doesn't depend on Gloss) (?)
transform :: Vector -> Vector -> Picture -> Picture
transform (sx:+sy) (dx:+dy) = scale sx sy . translate dx dy