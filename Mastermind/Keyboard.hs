module Mastermind.Keyboard (
ArrowKey(..),
getKey,
) where

import System.IO

{-
 - Custom system for parsing
 - arrow keys as input.
 -
 - Keys are all read as "\ESC[<l>",
 - where <l> is one of 'A','B','C', and 
 - 'D'. A little awkward, and, admittedly,
 - not a perfect solution. But it works
 - well enough.
-}


-- Data type for Arrow Keys.
-- L,R rather than Left,
-- Right to avoid conflict
-- with Either monad
data ArrowKey = U | D | L | R deriving Show


-- Parses an Arrow key from stdin
-- If arrow key pressed: IO (Just ArrowKey)
-- If no arrow pressed:  IO Nothing
getKey :: IO (Maybe ArrowKey)
getKey = do
	hSetEcho stdout False
	hSetBuffering stdin NoBuffering
	c <- getChar
	case c of
		'\ESC'  -> parseArrow
		'\n'	-> return Nothing
		_	-> getKey


-- Helper function for getKey
parseArrow :: IO (Maybe ArrowKey)
parseArrow = do
	c <- getChar
	case c of 
		'\ESC' -> parseArrow
		'[' -> do l <- getChar
			  case l of 
				'A' -> return $ Just U
				'B' -> return $ Just D
				'C' -> return $ Just R
				'D' -> return $ Just L
				_   -> return Nothing
		_   -> return Nothing
