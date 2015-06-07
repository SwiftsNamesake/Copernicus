-- |
-- Module      : LenseGrinder
-- Description : Sandbox for lens-related experiments
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental
-- Portability : POSIX (not sure)
-- 

-- Created June 4 2015

-- TODO | - 
--        - 

-- SPEC | -
--        -



{-# LANGUAGE TemplateHaskell #-}



module LenseGrinder where



---------------------------------------------------------------------------------------------------
-- We'll need these
---------------------------------------------------------------------------------------------------
import Control.Lens



---------------------------------------------------------------------------------------------------
-- Types
---------------------------------------------------------------------------------------------------
type Degrees = Double
type Latitude = Degrees
type Longitude = Degrees

data Meetup = Meetup { _name :: String, _location :: (Latitude, Longitude) }

makeLenses ''Meetup



---------------------------------------------------------------------------------------------------
-- Functions
---------------------------------------------------------------------------------------------------
