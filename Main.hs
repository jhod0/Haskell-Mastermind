module Main where

import Mastermind.Game
import Mastermind.Peg (codePegs)
import Mastermind.Board (exact, almost)


introMessage :: String
introMessage = unlines ["============================================",
	 "\tWelcome to MASTERMIND!",
	"============================================",
	"",
	"Your objective will be to guess my pattern of four colors.",
	"There are six possible colors: " ++ show codePegs,
	"",
	"Use the arrow keys to change your guess. Up/down arrows to change the selected peg,",
	"left and right to switch pegs. Any other key will enter your guess.",
	"",
	"You will receive a " ++ show exact ++ " for each peg in a correct location, and ",
	"a " ++ show almost ++ " for each good peg in the wrong location.",
	"",
	"For more information: http://en.wikipedia.org/wiki/Mastermind_%28board_game%29",
	"",
	"Good luck!",
	"",
	""]


greeting :: IO ()
greeting = putStrLn introMessage


main :: IO ()
main = do
	greeting
	putStrLn "Press any key to begin..."
	getChar
	playGame
	putStrLn ""
