definition module shares

import iTasks
import System.FilePath
import qualified Data.Map as DM

:: Settings = 	{ dirCpm 	:: FilePath
				, dirClean	:: FilePath
				, dirClean2 :: FilePath
				, dirIDEEnvs :: FilePath
				, dirCmd :: FilePath
				}
				
derive class iTask Settings

settings :: Shared Settings
errorstate :: Shared String

//contents = ([(filename,[line])],prev_time)
contents :: Shared (Map String [String])

project :: Shared String

//functions to get specific content
contentLinesOf :: String -> Task [String]
joinWithNewline :: String String -> String
contentOf :: String -> Task String