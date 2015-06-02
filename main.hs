-- |
-- Module      : main
-- Description : Entry point for the Copernicus project
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



module Main where



---------------------------------------------------------------------------------------------------
-- We'll need these
---------------------------------------------------------------------------------------------------
import GlossGraphics



---------------------------------------------------------------------------------------------------
-- Entry point
---------------------------------------------------------------------------------------------------
main :: IO ()
main = do
	simulate
	putStrLn "Finished"