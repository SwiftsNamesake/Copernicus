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
import Graphics.Gloss.Geometry.Angle (degToRad, radToDeg, normaliseAngle)
import Graphics.Gloss.Interface.IO.Game



---------------------------------------------------------------------------------------------------
-- Definitions
---------------------------------------------------------------------------------------------------




---------------------------------------------------------------------------------------------------
-- Entry point
---------------------------------------------------------------------------------------------------
main :: IO ()
main = do
	putStrLn "Hello World"