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

printHelp :: IO ()
printHelp = putStr helpText

versionText = unlines
				[ "extstat v1.0" ]

printVersion :: IO ()
printVersion = putStr versionText

data Options = Options	{ optVersion :: Bool
						, optHelp :: Bool
						, optAll :: Bool
						, optHuman :: Bool
						, optExt :: Bool
						, optCount :: Bool
						, optTotal :: Bool
						} deriving (Show)

defaultOptions :: Options
defaultOptions = Options { optVersion	= False
						 , optHelp		= False
						 , optAll		= False
						 , optTotal		= False
						 , optExt		= False
						 , optCount		= False
						 , optHuman		= False
						 }

options :: [OptDescr (Options -> IO Options)]
options = 
		[ Option 	"h" 	["help"]	   		(NoArg (\opts -> return opts { optHelp = True} )) 			"Help"
		, Option 	"a" 	["all"]    			(NoArg (\opts -> return opts { optVersion = True} )) 		"Hidden files"
		, Option 	"v" 	["version"] 		(NoArg (\opts -> return opts { optVersion = True} )) 		"Version"
		, Option 	"e"	 	["extension"] 		(NoArg (\opts -> return opts { optVersion = True} )) 		"Sort by extensions"
		, Option 	"c" 	["count"] 			(NoArg (\opts -> return opts { optVersion = True} )) 		"Sort by count of files"
		, Option 	"t" 	["total"]			(NoArg (\opts -> return opts { optVersion = True} )) 		"Show total number of files"
		]

main = do
	args <- getArgs
	let (actions, nonOpts, errors) = getOpt Permute options args
	opts <- foldl (>>=) (return defaultOptions) actions
	let Options { optVersion = version
				, optHelp = help
				, optCount = count
				, optExt = ext
				, optTotal = total
				, optHuman = human 
				, optAll = all } = opts	
	when help printHelp
	when version printVersion