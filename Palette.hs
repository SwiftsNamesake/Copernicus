-- |
-- Module      : Palette
-- Description : Colours for Cairo
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental
-- Portability : POSIX (not sure)
-- 
-- Jonatan H Sundqvist
-- June 11 2015
--



module Palette where



---------------------------------------------------------------------------------------------------
-- We'll need these
---------------------------------------------------------------------------------------------------
import qualified Graphics.Rendering.Cairo as Cairo



---------------------------------------------------------------------------------------------------
-- Types
---------------------------------------------------------------------------------------------------
type Colour = (Double, Double, Double, Double)

---------------------------------------------------------------------------------------------------
-- Colours
---------------------------------------------------------------------------------------------------
black  = (0, 0, 0, 1) :: Colour
white  = (1, 1, 1, 1) :: Colour
red    = (1, 0, 0, 1) :: Colour
green  = (0, 1, 0, 1) :: Colour
blue   = (0, 0, 1, 1) :: Colour



---------------------------------------------------------------------------------------------------
-- Functions
---------------------------------------------------------------------------------------------------
choose :: (Double, Double, Double, Double) -> Cairo.Render ()
choose (r, g, b, a) = Cairo.setSourceRGBA r g b a