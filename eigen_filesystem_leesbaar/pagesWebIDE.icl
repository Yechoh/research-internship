module pagesWebIDE

//import iTasks
import pageChooseFile
import pageEditor
import pageAskImportPaths
import qualified Data.Map as DM
import shares
import settings

Start :: *World -> *World
Start world = startEngine
	(startTask)
	world
	
// first force settings after which by default we open an editor onn the main .icl file
startTask :: Task ()
startTask 
	=				get settings
	>>- \curSet ->	get project
	>>- \curProj ->	let isProj = curProj.projectPath <> "" && curProj.projectName <> ""
					    isCPM  = curSet.cpmDirectory <> ""
					in if (not isProj || not isCPM) 
							(setSettings  >>| startTask)
					   		(pagenodeEditor) 	
	
pagenodeChooseFile :: Task ()
pagenodeChooseFile =
	pageChooseFile (ActionContinue, pagenodeEditor)

pagenodeEditor :: Task ()
pagenodeEditor =
	get contents >>- \c.
	if ('DM'.mapSize c == 0)
	pagenodeChooseFile
	(pageEditor
	(	(ActionOpen,
			pagenodeChooseFile)
	,	(Action "import paths",
			pagenodeAskImportPaths)
	))
		
pagenodeAskImportPaths :: Task ()
pagenodeAskImportPaths=
	pageAskImportPaths
	(	(ActionContinue,
			pagenodeEditor)
	,	(ActionCancel,
			pagenodeEditor)
	)
	


