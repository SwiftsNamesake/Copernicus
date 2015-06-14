-- |
-- Module      : Core
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
--        - Polymorphic types (not just Floats) (?)

-- SPEC | -
--        -



module Copernicus.Core where



---------------------------------------------------------------------------------------------------
-- We'll need these
---------------------------------------------------------------------------------------------------
import Data.Complex
-- import Control.Monad (when)
-- import Graphics.Gloss.Geometry.Angle (degToRad, radToDeg, normaliseAngle)



---------------------------------------------------------------------------------------------------
-- Types
---------------------------------------------------------------------------------------------------
-- Make typeclass (mesh, body, bounding box, collision, etc) (?)
-- type Number a = Floating a -- Real number
type Vector = Complex -- TODO: Polymorphic (cf. related TODO item)
data Body f = Body (Vector f) (Vector f) (Vector f) deriving Show -- Add argument



---------------------------------------------------------------------------------------------------
-- Functions
---------------------------------------------------------------------------------------------------
-- Accessors --------------------------------------------------------------------------------------
-- |
-- TODO: Replace with lenses
position :: Body f -> Vector f
position (Body p _ _) = p

velocity :: Body f -> Vector f
velocity (Body _ v _) = v

acceleration :: Body f -> Vector f
acceleration (Body _ _ a) = a



---------------------------------------------------------------------------------------------------
-- |
-- clampAngle :: Float -> Float
-- clampAngle = radToDeg . normaliseAngle . degToRad



-- |
-- TODO: Make them change colour when bouncing (?)
animate :: (RealFloat f, Floating f) => f -> Body f -> Body f
animate t (Body p v a) = collide ground $ Body (parabola t p v a) (v + (t:+0)*a) a
	where ground = 0.0 -- 30+30/2-540/2 (previous hard-coded value)



-- | Moves a point along the given parabola for the given amount of time
parabola :: (RealFloat f, Floating f) => f -> Vector f -> Vector f -> Vector f -> Vector f
parabola t (px:+py) (vx:+vy) (ax:+ay) = (px + vx*t + 0.5*ax*t**2) :+ (py + vy*t + 0.5*ay*t**2)



-- | collide
-- Very primitive for now
-- TODO: Use 'contains' function (Range -> Value -> Bool)
-- TODO: Don't hard-code bounds (left, right)
-- TODO: Take bounds of Body into account (don't hard-code that either)
collide :: (RealFloat f, Floating f) => f -> Body f -> Body f
collide gnd (Body (px:+py) (vx:+vy) a) = Body (px:+py) ((invertIf (\ _ -> (px <= left) || ( px >= right)) vx) :+ (invertIf (\ v -> (v < 0) && (py <= gnd)) vy)) a
	where invertIf p v | p v 	   = -v
	                   | otherwise =  v
	      (left, right) = (-5, 5) --(15-720/2, 720/2-30/2)



-- | ETA (estimated time of arrival), given the inital position (p') and velocity (v')
--   and the acceleration.
-- TODO: Deal with v=0, a=0 (✓)
-- TODO: Take collisions into account, both axis (?)
-- TODO: Rename (eg. timeUntil, solveForT, predict, etc)
-- TODO: Simplify if possible
-- TODO: Parabola type (eg. Parabola a v x)
-- eta :: Acceleration
solveParabola :: (RealFloat f, Floating f) => f -> f -> f -> f -> Maybe f
solveParabola p' 0  0  p
	| p' == p   = Just 0
	| otherwise = Nothing

solveParabola p' v' 0  p
	| (v' == 0) && (p' /= p) = Nothing
	| otherwise              = Just $ (p - p')/v'

solveParabola p' v' a' p
	| discriminant > 0  = Just $ (-v'/a') + sqrt discriminant
	| otherwise         = Nothing
	where discriminant = (v'/a')**2 - 2*(p' - p)/a' -- We're not interested in imaginary solutions



-- Utilities (should eventually be moved to separate module or library ----------------------------
-- Python-style String formatting (eg. keyword interpolation, {0}, customisation, format specs.)
-- Parsec



---------------------------------------------------------------------------------------------------
-- Entry point
---------------------------------------------------------------------------------------------------