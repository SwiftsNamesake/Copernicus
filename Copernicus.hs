-- |
-- Module      : Copernicus
-- Description : Contains the core of the Copernicus project
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental
-- Portability : POSIX (not sure)
-- 
-- Jonatan H Sundqvist
-- November 25 2014
--

-- TODO | - Model planetary motion and physics in 2D
--        - Cartoon earth with gravitational field
--        - Flexible event handling
--        - App typeclass (run, manage events, window properties, etc.)
--        - Lenses
--        - Options (eg. toggle grid) (cf. 'when')
--        - UI
--        - Use types to encode units (eg. SI, radians)
--        - Move to Cairo (branch?)

-- SPEC | -
--        -



module Copernicus where



---------------------------------------------------------------------------------------------------
-- We'll need these
---------------------------------------------------------------------------------------------------
import Data.Complex
-- import Control.Monad (when)

import Graphics.Gloss.Geometry.Angle (degToRad, radToDeg, normaliseAngle)



---------------------------------------------------------------------------------------------------
-- Types
---------------------------------------------------------------------------------------------------
-- Make typeclass (mesh, body, bounding box, collision, etc) (?)
type Vector = Complex Float
data Body   = Body Vector Vector Vector -- Add argument



---------------------------------------------------------------------------------------------------
-- Functions
---------------------------------------------------------------------------------------------------
clampAngle :: Float -> Float
clampAngle = radToDeg . normaliseAngle . degToRad



--
-- TODO: Make them change colour when bouncing (?)
animate :: Float -> Body -> Body
animate t (Body p v a) = collide (30+30/2-540/2) $ Body (parabola t p v a) (v + (t:+0)*a) a



--
parabola :: Float -> Vector -> Vector -> Vector -> Vector
parabola t p v a = let (px:+py) = p
                       (vx:+vy) = v
                       (ax:+ay) = a
                   in (px + vx*t + 0.5*ax*t**2) :+ (py + vy*t + 0.5*ay*t**2)



-- collide
-- Very primitive for now
-- TODO: Use 'contains' function (Range -> Value -> Bool)
collide :: Float -> Body -> Body
collide gnd (Body (px:+py) (vx:+vy) a) = Body (px:+py) ((invertIf (\ v -> (px <= 15-720/2) || ( px >= (720/2-30/2))) vx) :+ (invertIf (\ v -> (v < 0) && (py <= gnd)) vy)) a
	where invertIf p v
		| p v 		= -v
		| otherwise =  v



-- | ETA (estimated time of arrival)
-- TODO: Rename (eg. timeUntil, solveForT, etc)
-- TODO: Parabola type (eg. Parabola a v x)
-- eta :: Acceleration



-- Utilities (should eventually be moved to separate module or library ----------------------------
-- Python-style String formatting (eg. keyword interpolation, {0}, customisation, format specs.)
-- Parsec



---------------------------------------------------------------------------------------------------
-- Entry point
---------------------------------------------------------------------------------------------------