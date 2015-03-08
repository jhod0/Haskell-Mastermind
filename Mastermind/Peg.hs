module Mastermind.Peg (
Peg(..),
codePegs,
) where

import Data.Char

import Mastermind.Colors

data Peg = Peg Color deriving (Eq, Ord)

-- 9278, or 0x2699, 
-- is utf8 for a star
star :: String
star = [chr 9728]


-- Pegs will be shown as stars
instance Show Peg where 
	show (Peg Black) = show Bold ++ show Whitebgrnd
				++ show Black ++ star ++ " "
				++ show ColorEnd
	show (Peg c) = show Bold ++ show c 
			++ star ++ " " ++ show ColorEnd


-- Pegs should be one of six colors:
-- Red, Green, Yellow, Blue, Violet, Turq
codePegs :: [Peg]
codePegs = map Peg [Red,Green,Yellow,Blue,Violet,Turq]
