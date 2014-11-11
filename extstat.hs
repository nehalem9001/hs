--Haskell--
import System.Console.GetOpt
import System.Exit
import System.Posix.Files
import Data.List
import System.Environment
import System.Directory
import Control.Monad
import System.FilePath.Posix

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
		[ Option 	"h" 	["help"]	   		(NoArg (\opts -> return opts { optHelp = True} )) 		"Help"
		, Option 	"a" 	["all"]    			(NoArg (\opts -> return opts { optAll = True} )) 		"Include hidden files"
		, Option 	"v"		["version"]			(NoArg (\opts -> return opts { optVersion = True} ))	"Version"
		, Option 	"e"		["extension"]		(NoArg (\opts -> return opts { optExt = True} )) 		"Sort by extensions"
		, Option 	"c" 	["count"] 			(NoArg (\opts -> return opts { optCount = True} )) 		"Sort by count of files"
		, Option 	"t" 	["total"]			(NoArg (\opts -> return opts { optTotal = True} )) 		"Show total number of files"
		, Option 	"t" 	["human"]			(NoArg (\opts -> return opts { optHuman = True} )) 		"Show sizes in human readable form"
		]

getDirectories :: [FilePath] -> (String -> Bool) -> IO [FilePath]
getDirectories [] _ = return []
getDirectories fplist flt = liftM2 (++) files $ liftM (filter (\x -> x /= "." && x /= ".." && flt x)) dirs >>= flip getDirectories flt
							where
								dirs 	= filterDirectoryContent fp flt doesDirectoryExist
								files 	= filterDirectoryContent fp flt doesFileExist
								fp 		= map (\p -> if last p /= '/' then p ++ "/" else p) fplist


filterDirectoryContent :: [FilePath] -> (String -> Bool) -> (FilePath -> IO Bool) -> IO [FilePath]
filterDirectoryContent [] flt _ = return []
filterDirectoryContent fp flt g = liftM concat 
										(filterM doesDirectoryExist fp 
											>>= mapM (\x -> liftM (map (x++)) (liftM (filter (\x -> x /= "." && x /= ".." && flt x)) (getDirectoryContents x))))
											>>= filterM g

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
	
	when help (printHelp >> exitSuccess)
	when version (printVersion >> exitSuccess)

	let includeHidden = if all then (const True) else (\x -> head x /= '.')
	let dirs = last args
	paths <- getDirectories [dirs] includeHidden
	let extensions = groupBy (\p1 p2 -> snd p1 == snd p2).sortBy (\u v -> compare (snd u) (snd v)).filter (not.null.snd) $ map splitExtension paths
	let sizes = map (\p -> (snd (head p), length p)) extensions
	
	fileSizes <- mapM getFileStatus paths	
	mapM (\p -> putStrLn (show (fst p) ++ " " ++ show (snd p))) (sortBy (\u v -> compare (snd u) (snd v)) sizes)