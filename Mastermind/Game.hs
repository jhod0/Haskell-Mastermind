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
getPegs gen = getPegs' gen 4 
	where getPegs' _ 0 = []
	      getPegs' g num = let (n,ng) = randomR (0,nPegs-1) g in 
				codePegs !! n:getPegs' ng (num-1)

-- Generates a new board
newBoard :: IO Board
newBoard = do
		g <- newStdGen
		let [p1,p2,p3,p4] = getPegs g
		return $ Board p1 p2 p3 p4



-----------------------------------------------
-------------     THE GAME       --------------
-----------------------------------------------


boardFromNs :: [Int] -> Board
boardFromNs ls = if length ls /= 4 then error $ "Wrong board size: " ++ show ls else
		let [p1,p2,p3,p4] = map (codePegs!!) ls in
			Board p1 p2 p3 p4


applytoN :: (a -> a) -> Int -> [a] -> [a]
applytoN _ _   []   = []
applytoN f 0 (a:xs) = f a:xs
applytoN f n (a:xs) = a:applytoN f (n-1) xs

incN :: Int -> [Int] -> [Int]
incN = applytoN (\n -> (n+1) `mod` nPegs)

decN :: Int -> [Int] -> [Int]
decN = applytoN (\n -> (n-1) `mod` nPegs)

-- printBoard prints a board with the nth
-- peg underlined
printBoard :: Int -> Board -> IO ()
printBoard n b = putStr $ "\r\t" ++ underlineNth n b

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
