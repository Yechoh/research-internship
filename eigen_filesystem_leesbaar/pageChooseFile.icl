implementation module pageChooseFile

import iTasks
import pagetypes
import directoryBrowsing
import shares
import extraTaskCombinators
import callCpm

pageChooseFile :: ChooseFileRedirects -> Task ()
pageChooseFile pagenodeEditor =
	askPath					
	-||
	updateSharedInformation "paths" [] settings
	>>- \(path,name) -> pagenodeEditor path name

askPath :: Task (String,String)
askPath
	=							getPwdName
	>>- \pwd ->					selectFromTree pwd isCleanFile
	>>- \path -> return (takeDirectory path, dropDirectory path)

setContent :: String String -> Task ()
setContent path name
	= 							readFromFile (path </> name)
	>>- \(Just contenttxt) ->		set contenttxt content
								>>|- return ()
	
setProject :: String String -> Task ()
setProject path name
	=							readFromFile (toproj (path </> name))
	>>- \mprojtxt ->			(case mprojtxt of
		Nothing ->					(cpmCreateProject (toproj name) 
									>>|- return ())
		(Just projtxt) ->			return ()
								)

