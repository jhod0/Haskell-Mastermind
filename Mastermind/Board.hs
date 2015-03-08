module Mastermind.Board (
Board(..),
boardDiff,
underlineNth,
exact,
almost,
) where

import qualified Data.Map as M

import Mastermind.Colors
import Mastermind.Peg



-----------------------------------------------
--------   Board data type and related    -----
-----------------------------------------------
data Board = Board Peg Peg Peg Peg deriving Eq

instance Show Board where
	show (Board p1 p2 p3 p4) = "[ " ++ unwords (map show [p1,p2,p3,p4])
						++ " ]"

-- An empty map of pegs
emptyMap :: M.Map Peg Int
emptyMap = M.fromList $ zip codePegs [0,0,0,0,0,0]

-- Converts a board into a Data.Map of pegs
boardToMap :: Board -> M.Map Peg Int
boardToMap (Board p1 p2 p3 p4) = foldl (flip (M.adjust (+1)))
				  emptyMap [p1,p2,p3,p4]


-- Returns a string with the Board
-- which has the Nth peg underlined.
-- Is used in Mastermind.Game.printBoard
underlineNth :: Int -> Board -> String
underlineNth n b@(Board p1 p2 p3 p4) = case n of 
					0 -> "[ " ++ unwords [show Underline ++ show p1, show p2, show p3, show p4] ++ " ]"
					1 -> "[ " ++ unwords [show p1, show Underline ++ show p2, show p3, show p4] ++ " ]"
					2 -> "[ " ++ unwords [show p1, show p2, show Underline ++ show p3, show p4] ++ " ]"
					3 -> "[ " ++ unwords [show p1, show p2, show p3, show Underline ++ show p4] ++ " ]"
					_ -> show b


-------------------------------------------------
-----          Scoring system             -------
-------------------------------------------------


-- Response to a good peg in the right place
-- is a Black peg
exact :: Peg
exact = Peg Black

-- Response to a good peg in the wrong place
-- is a White peg
almost :: Peg
almost = Peg White


nOverlap :: Board -> Board -> Int
nOverlap guess key = let gmap = boardToMap guess
			 kmap = boardToMap key in
			sum $ map snd $ M.toList $ 
			M.intersectionWith (\g k -> if g == 0
					then 0
					else case compare g k of
						GT -> k
						EQ -> k
						LT -> g) gmap kmap
-- Yields the number of exactly correct pegs,
-- i.e., pegs which have the correct color
-- and correct position.
nExact :: Board -> Board -> Int
nExact (Board g1 g2 g3 g4) 
       (Board k1 k2 k3 k4) = sum $ map (\(p1,p2) -> if p1 == p2 then 1 else 0)
				       [(g1,k1),(g2,k2),(g3,k3),(g4,k4)]

-- Yields the number of pegs which are the 
-- correct color, but in the wrong position
nAlmost :: Board -> Board -> Int
nAlmost guess key = let e = nExact guess key
			o = nOverlap guess key
			in o - e

-- Determine what 'score' the guess is, i.e., what
-- pattern of 'exact' and 'almost' pegs to respond with
boardDiff :: Board -> Board -> [Peg]
boardDiff guess key = replicate (nExact guess key) exact 
			++ replicate (nAlmost guess key) almost
