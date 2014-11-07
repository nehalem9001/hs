--Haskell--
import System.Console.GetOpt
import Control.Monad
import Control.Applicative
import Data.List
import Data.Map

data Flag = Verbose
			| Help
			| All
			| Human
			| ByExt
			| ByCount
			| Total

data Options = Options

options :: [OptDescr Flag]
options = 
		[	Option ['v'] ["verbose"] (NoArg Verbose) "verbose"
		,   Option ['h'] ["help"]   (NoArg Help) "help"
		,   Option ['a'] ["all"]    (NoArg All) "all"
		]

str :: String -> String
str s = s

main = do interact str
