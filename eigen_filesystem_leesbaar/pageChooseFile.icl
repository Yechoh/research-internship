implementation module pageChooseFile

import iTasks
import pagetypes
import directoryBrowsing
import shares
import extraTaskCombinators
import callCpm
import qualified Data.Map as DM

pageChooseFile :: ChooseFileRedirects -> Task ()
pageChooseFile (actionContinue,pagenodeEditor) =
	askPath					
	-||
	updateSharedInformation "paths" [] settings
	>>- \(path,name) -> setContents path name >>| setProject path name >>| (pagenodeEditor path name)

askPath :: Task (String,String)
askPath
	=							getPwdName
	>>- \pwd ->					selectFromTree pwd isCleanFile
	>>- \path -> return (takeDirectory path, dropDirectory path)

setContents :: String String -> Task ()
setContents path name
	= 							readLinesFromFile (path </> name)
	>>- \(Just contenttxt) ->	get contents
	>>- \contentmap ->			set ('DM'.put name contenttxt contentmap) contents
								>>|- return ()
	
setProject :: String String -> Task ()
setProject path name
	=							readFromFile (toproj (path </> name))
	>>- \mprojtxt ->			(case mprojtxt of
		Nothing ->					(cpmCreateProject (toproj name) 
									>>|- return ())
		(Just projtxt) ->			return ()
								)

