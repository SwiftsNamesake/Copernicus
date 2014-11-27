--
-- Main.hs - Copernicus
-- Entry point for the Copernicus project
--
-- Jonatan H Sundqvist
-- November 25 2014
--

-- TODO | - Model planetary motion and physics in 2D
--        -

-- SPEC | -
--        -



module Main where



---------------------------------------------------------------------------------------------------
-- We'll need these
---------------------------------------------------------------------------------------------------
import Graphics.Gloss.Data.Picture (line)
import Graphics.Gloss (circleSolid)
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
parabola t v a p = let (px,py) = p; (vx,vy) = v; (ax,ay) = a in (px + vx*t + 0.5*ax*t**2, py + vy*t + 0.5*ay*t**2)


-- Make typeclass (mesh, body, bounding box, collision, etc) (?)
data Body = Body Vector Vector Vector


--
animate :: Float -> Body -> Body
animate t (Body p v a) = Body (parabola t v a p) (v + mulSV t v) a



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
		display  			  = InWindow "Simulator" (740, 540) (25, 25)
		world 	 			  = (0 :+ 0, v)
		render (re:+im, _) 	  = return $ pictures [color red . translate (re) (im) $ circleSolid 15]
		respond e w 		  = return w
		advance t w = return $ (parabola t (snd w) g (fst w), snd w + (t:+0.0)*g)
		v 					  = 40.0 :+ 20.0
		g 					  = 0.0 :+ (-9.82)
		parabola t v a p = let (px:+py) = p; (vx:+vy) = v; (ax:+ay) = a in (px + vx*t + 0.5*ax*t**2):+(py + vy*t + 0.5*ay*t**2)




---------------------------------------------------------------------------------------------------
-- Entry point
---------------------------------------------------------------------------------------------------
main :: IO ()
main = do
	simulate