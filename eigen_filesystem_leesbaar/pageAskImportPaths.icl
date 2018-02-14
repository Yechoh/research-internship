implementation module pageAskImportPaths

import iTasks
import pagetypes
import shares
import directoryBrowsing
import extraTaskCombinators
import qualified Data.Map as DM
import System.OS
import Text
import iTasks.UI.Editor.Builtin
import errorHandling
import pageEditor

pageAskImportPaths :: Task ()
pageAskImportPaths =
	get project >>- \proj.
	readFromFile ((proj.projectPath </> proj.projectName)+++".prj") >>- \mprojtxt. case mprojtxt of
	Nothing = viewInformation "" [] (proj.projectPath +++ "   " +++ proj.projectName) >>| return ()
	(Just projtxt) =
		showUnresolvedImports
		||-
		showMapSelector proj.projectName
		>>* [   OnAction  (Action "Continue")   	(always (pageEditor))
			,	OnAction (Action "Cancel")		(always (writeToFile (proj.projectName) projtxt >>|- pageEditor))
			]

Errors2Imports :: [String] -> String
Errors2Imports lines
	# importlines = filter (\line. endsWith "imported" line) lines
	# importlinessplitted = map (split " ") importlines
	# dclnames = map (\x. hd (tl (tl x))) importlinessplitted
	=  join "\n" dclnames

showUnresolvedImports :: Task ()
showUnresolvedImports =
	viewSharedInformation "Unresolved imports"  [ViewUsing Errors2Imports (textArea 'DM'.newMap)] errorStore >>|- return ()

addPath2Project :: String String String -> String
addPath2Project path cleandir projtxt
	# short_path = dropFileName (subString (textSize cleandir) (textSize path) path)
	# newpath = "\t\tPath:\t{Application}" +++ short_path
	# splitted_projtxt = split ("Path:\t{Project}"+++OS_NEWLINE) projtxt
	# paths = hd (tl splitted_projtxt)
	# newpaths = newpath +++ OS_NEWLINE +++ paths
	# newprojtxt = join ("Path:\t{Project}"+++OS_NEWLINE) [(hd splitted_projtxt),newpaths]
	= newprojtxt

showMapSelector :: String -> Task ()
showMapSelector projname =
	get settings
	>>= \settings. selectFromTree True settings.cleanHome (isFile "dcl")
	>>= \(dclpath,dclname). readFromFile projname
	>>- \(Just projtxt). saveFile projname (addPath2Project dclpath settings.cleanHome projtxt)
	>>|- build
	>>|- showMapSelector projname
