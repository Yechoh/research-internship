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
	>>- \miclloc -> case miclloc of
		Nothing = pagenodeEditor
		(Just (a,b)) = setContents (a </> b) >>|- /*setProject iclloc >>|*/ (pagenodeEditor)

askPath :: Task (Maybe (String,String))
askPath
	=							getPwdName
	>>- \pwd ->					(selectFromTreeMaybe True pwd isCleanFile)
	//>>- \(a,b)->				return (Just (a </> b))
	//>>- \path -> return (takeDirectory path, dropDirectory path)

setContents :: String -> Task ()
setContents iclloc
	= 							readLinesFromFile (iclloc)
	>>- \mct -> case mct of
		Nothing = viewInformation "" [] iclloc >>| return ()
		(Just contenttxt) =	get contents
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
