module Mastermind.Game (
playGame,
) where

import System.Random
import System.IO

import Mastermind.Board
import Mastermind.Peg
import Mastermind.Keyboard
import Mastermind.Colors



--------------------------------------------------
--              RANDOM Stuff 	                --
--------------------------------------------------
-- Utilities for randomly generating boards, etc.-
-- -----------------------------------------------


-- The number of possible pegs
-- For flexibility, so I can simply change
-- the pegs in Peg.hs
nPegs :: Int
nPegs = length codePegs


-- Returns a list of four
-- randomly selected pegs
getPegs :: StdGen -> [Peg]
getPegs gen = fmap (codePegs !!) randomPegs
	where randomPegs = take 4 $ randomRs (0,3) gen

-- Generates a new board
newBoard :: IO Board
newBoard = do
		g <- newStdGen
		let [p1,p2,p3,p4] = getPegs g
		return $ Board p1 p2 p3 p4



-----------------------------------------------
-------------     THE GAME       --------------
-----------------------------------------------


-- Overall structure:
-- 	The user-chosen board is modelid as a list of 
-- Ints, where 0 means the first Peg in codePegs (from
-- Mastermind.Pegs), 1 means the second from codePegs,
-- and so on.


-- Converts a list of four Ints into a 
-- Mastermind board
boardFromNs :: [Int] -> Board
boardFromNs ls = if length ls /= 4 then error $ "Wrong board size: " ++ show ls else
		let [p1,p2,p3,p4] = map (codePegs!!) ls in
			Board p1 p2 p3 p4


-- The funcs incN and decN just shift
-- the color at a given position
-- in a board-list.
modifyAt :: (a -> a) -> Int -> [a] -> [a]
modifyAt f ind l = 
	case splitAt ind l of 
	(prefix, x:suffix) -> prefix ++ f x :suffix
	(prefix, [])       -> prefix


incN :: Int -> [Int] -> [Int]
incN = modifyAt (\n -> (n+1) `mod` nPegs)

decN :: Int -> [Int] -> [Int]
decN = modifyAt (\n -> (n-1) `mod` nPegs)



-- printBoard prints a board with the nth
-- peg underlined
printBoard :: Int -> Board -> IO ()
printBoard n b = putStr $ "\r\t" ++ underlineNth n b


-- 	Allows the user to alter their guess board.
-- Argument <n> is which board position is currently highlighted,
-- and <b> is a [Int] specifying the current board guess.
chooseBoard :: Int -> [Int] -> IO [Int]
chooseBoard n b = do
		printBoard n $ boardFromNs b
		hFlush stdout
		u <- getKey
		case u of 
			Just a -> case a of
				U -> chooseBoard n $ incN n b
				D -> chooseBoard n $ decN n b
				L -> chooseBoard ((n-1) `mod` 4) b
				R -> chooseBoard ((n+1) `mod` 4) b
			Nothing -> return b



-- 	Takes a Board key, an initial guess [Int], and
-- a turn number, and plays the game until the user
-- guesses the key.
playBoard :: Board -> [Int] -> Int -> IO ()
playBoard key guess n = do
	putStrLn $ "============ Turn " ++ show n ++ " ==========="
	g <- chooseBoard 0 guess
	let thisGuess = boardFromNs g
	if thisGuess == key
		then do
			putStrLn "\n\n"
			putStrLn $ "Congratulations! You won in " ++ show n ++ " moves!"
			putStrLn $ "The key was: " ++ show key
			return ()
		else do
			let diff = boardDiff thisGuess key
			putStrLn $ "\r\t" ++ show thisGuess
			putStrLn $ concatMap show diff
			putStrLn ""
			playBoard key g (1+n)


-- Generates a new board and plays Mastermind
playGame :: IO ()
playGame = do
		b <- newBoard
		playBoard b [0,0,0,0] 1
