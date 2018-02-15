implementation module pageChooseFile

import iTasks
import directoryBrowsing
import shares
import extraTaskCombinators
//import callCpm
import qualified Data.Map as DM
import extraText
import System.OS
import pageEditor
import content
import dcls

pageChooseFile :: Task ()
pageChooseFile =
	askPath
	-||
	updateSharedInformation "paths" [] settings
	>>- \miclloc -> case miclloc of
		Nothing = pageEditor
		(Just (a,b)) = setContents (a </> b) >>|- /*setProject iclloc >>|*/ (pageEditor)

askPath :: Task (Maybe (String,String))
askPath
	=							get project
	>>- \p ->					(selectFromTreeMaybe True p.projectPath isCleanFile)
	//>>- \(a,b)->				return (Just (a </> b))
	//>>- \path -> return (takeDirectory path, dropDirectory path)






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
