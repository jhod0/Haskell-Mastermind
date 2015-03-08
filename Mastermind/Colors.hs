module Mastermind.Colors (
Color(..),
) where


data Color = ColorEnd | Bold | Weak | Italic | Underline | Strikethrough
			| Blackbgrnd | Redbgrnd | Yellowbgrnd | Bluebgrnd
			| Violetbgrnd | Turqbgrnd | Whitebgrnd | Greenbgrnd
			| DarkBlack | DarkRed | DarkGreen | DarkYellow | DarkBlue 
			| DarkViolet | DarkTurq | White | Black | Red | Green 
			| Yellow | Blue | Violet | Turq deriving (Eq, Ord)


escKey :: String
escKey = "\ESC"

instance Show Color where
	show ColorEnd = escKey ++ "[0m"
	show White = show ColorEnd
	
	show Bold = escKey ++ "[1m"
	show Weak = escKey ++ "[2m"
	show Italic = escKey ++ "[3m"
	show Underline = escKey ++ "[4m"
	show Strikethrough = escKey ++ "[9m"

	show DarkBlack = escKey ++ "[30m"
	show DarkRed = escKey ++ "[31m"
	show DarkGreen = escKey ++ "[32m"
	show DarkYellow = escKey ++ "[33m"
	show DarkBlue = escKey ++ "[34m"
	show DarkViolet = escKey ++ "[35m"
	show DarkTurq = escKey ++ "[36m"

	show Blackbgrnd = escKey ++ "[40m"
	show Redbgrnd = escKey ++ "[41m"
	show Greenbgrnd = escKey ++ "[42m"
	show Yellowbgrnd = escKey ++ "[43m"
	show Bluebgrnd = escKey ++ "[44m"
	show Violetbgrnd = escKey ++ "[45m"
	show Turqbgrnd = escKey ++ "[46m"
	show Whitebgrnd = escKey ++ "[47m"

	show Black = escKey ++ "[90m"
	show Red = escKey ++ "[91m"
	show Green = escKey ++ "[92m"
	show Yellow = escKey ++ "[93m"
	show Blue = escKey ++ "[94m"
	show Violet = escKey ++ "[95m"
	show Turq = escKey ++ "[96m"
