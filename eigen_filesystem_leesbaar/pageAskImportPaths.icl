implementation module pageAskImportPaths

import iTasks
import pagetypes
import shares
import directoryBrowsing
import extraTaskCombinators
import qualified Data.Map as DM
import callCpm
import System.OS
import Text
import iTasks.UI.Editor.Builtin
import callCpm

pageAskImportPaths :: String String AskImportPathsRedirects -> Task ()
pageAskImportPaths path name ((actioncontinue,pagenodeEditor),(actioncancel,pagenodeEditor2)) =
	readFromFile (path </> (toproj name)) >>- \(Just projtxt).
	showUnresolvedImports
	||-
	showMapSelector (path </> name)
	>>* [   OnAction  actioncontinue   	(always (pagenodeEditor path name))
		,	OnAction actioncancel		(always (writeToFile (path </> (toproj name)) projtxt >>|- pagenodeEditor2 path name))
		]
		
Errors2Imports :: String -> String
Errors2Imports errors 
	# lines = split OS_NEWLINE errors
	# importlines = filter (\line. endsWith "imported" line) lines
	# importlinessplitted = map (split " ") importlines
	# dclnames = map (\x. hd (tl (tl x))) importlinessplitted
	=  join "\n" dclnames

showUnresolvedImports :: Task String
showUnresolvedImports = 
	viewSharedInformation "Unresolved imports"  [ViewUsing Errors2Imports (textArea 'DM'.newMap)] errorstate

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
showMapSelector iclloc = 
	get settings
	>>= \settings. selectFromTree settings.dirClean2 (isFile "dcl")
	>>= \path. readFromFile (toproj iclloc)
	>>- \(Just projtxt). saveFile (toproj iclloc) (addPath2Project path settings.dirClean projtxt)
	>>|- cpmSetErrorstate (toproj iclloc)
	>>|- showMapSelector iclloc