implementation module pageChooseFile

import iTasks
import pagetypes
import directoryBrowsing
import shares
import extraTaskCombinators
//import callCpm
import qualified Data.Map as DM

pageChooseFile :: ChooseFileRedirects -> Task ()
pageChooseFile (actionContinue,pagenodeEditor) =
	askPath
	-||
	updateSharedInformation "paths" [] settings
	>>- \iclloc -> setContents iclloc >>| (pagenodeEditor)

askPath :: Task String
askPath
	=							getPwdName
	>>- \pwd ->					(selectFromTree True pwd isCleanFile)
	>>- \(a,b)->				return (a</>b)
	//>>- \path -> return (takeDirectory path, dropDirectory path)

setContents :: String -> Task ()
setContents iclloc
	= 							/*viewInformation "" [] iclloc >>|*/ readLinesFromFile iclloc
	>>- \(Just contenttxt) ->	get contents
	>>- \contentmap ->			set ('DM'.put iclloc contenttxt contentmap) contents
								>>|- return ()

//if there do not already exist	a temp icl and temp proj
	//create temp icl
	//if a proj already exist
		//base the temp proj on it
	//else
		//create new temp orj
/*setProject :: String String -> Task ()
setProject path name
	=							readFromFile (toproj (path </> name))
	>>- \mprojtxt ->			(case mprojtxt of
		Nothing ->					(cpmCreateProject (toproj name)
									>>|- return ())
		(Just projtxt) ->			return ()
								)
*/
