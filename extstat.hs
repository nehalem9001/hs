--Haskell--
import System.Console.GetOpt
import System.Environment
import Control.Monad

helpText :: String
helpText = unlines 
			[ "Usage exstat: [OPTIONS] directories..."
			, "          --help      display program options"
			, "          --version   display program version"
			, "    -l    --follow    follow symbolic links (disabled by default)"
			, "    -a    --all       include hidden files  (disabled by default)"			
			]	

data Options = Options	{ optVerbose :: Bool
						, optHelp :: Bool
						, optAll :: Bool
						, optHuman :: Bool
						, optExt :: Bool
						, optCount :: Bool
						, optTotal :: Bool
						} deriving (Show)

defaultOptions :: Options
defaultOptions = Options { optVerbose	= False
						 , optHelp		= False
						 , optAll		= False
						 , optTotal		= False
						 , optExt		= False
						 , optCount		= False
						 , optHuman		= False
						 }

printHelp :: IO ()
printHelp = putStrLn helpText

options :: [OptDescr (Options -> IO Options)]
options = 
		[ Option 	"h" 	["help"]	   		(NoArg (\opts -> return opts { optHelp = True} )) 			"Help"
		, Option 	"a" 	["all"]    			(NoArg (\opts -> return opts { optVerbose = True} )) 		"Hidden files"
		, Option 	"v" 	["verbose"] 		(NoArg (\opts -> return opts { optVerbose = True} )) 		"Verbose"
		, Option 	"e"	 	["extension"] 		(NoArg (\opts -> return opts { optVerbose = True} )) 		"Sort by extensions"
		, Option 	"c" 	["count"] 			(NoArg (\opts -> return opts { optVerbose = True} )) 		"Sort by count of files"
		, Option 	"t" 	["total"]			(NoArg (\opts -> return opts { optVerbose = True} )) 		"Show total number of files"
		]

main = do
	args <- getArgs
	let (actions, nonOpts, errors) = getOpt Permute options args
	opts <- foldl (>>=) (return defaultOptions) actions
	let Options { optVerbose = verbose
				, optHelp = help
				, optCount = count
				, optExt = ext
				, optTotal = total
				, optHuman = human 
				, optAll = all } = opts
	when help printHelp