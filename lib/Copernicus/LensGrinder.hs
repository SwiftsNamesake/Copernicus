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

-

{-# LANGUAGE TemplateHaskell #-}



module Copernicus.LensGrinder where



---------------------------------------------------------------------------------------------------
-- We'll need these
---------------------------------------------------------------------------------------------------
import Control.Lens
import Control.Monad.State (State, execState, get)



---------------------------------------------------------------------------------------------------
-- Types
---------------------------------------------------------------------------------------------------
type Degrees = Double
type Latitude = Degrees
type Longitude = Degrees

-- data Meetup = Meetup { _name :: String, _location :: (Latitude, Longitude) }
data Player = Player { _name :: String, _position :: (Double, Double), _health :: Int } deriving Show

-- makeLenses ''Meetup
makeLenses ''Player



---------------------------------------------------------------------------------------------------
-- Functions
---------------------------------------------------------------------------------------------------



---------------------------------------------------------------------------------------------------
-- Entry point
---------------------------------------------------------------------------------------------------
main :: IO ()
main = do
	let player = Player { _name="Jonatan", _position=(5, 13), _health=100 }
	print . flip execState player $ do
		health %= (*0.95)
	s <- getLine
	return ()