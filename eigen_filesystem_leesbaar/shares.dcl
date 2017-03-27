definition module shares

import iTasks
import System.FilePath

:: Settings = 	{ dirCpm 	:: FilePath
				, dirClean	:: FilePath
				, dirClean2 :: FilePath
				, dirIDEEnvs :: FilePath
				, dirCmd :: FilePath
				}
				
derive class iTask Settings

settings :: Shared Settings
errorstate :: Shared String
content :: Shared String