module pagesWebIDE

import iTasks
import pageChooseFile
import pageEditor
import pageAskImportPaths

Start :: *World -> *World
Start world = startEngine
	pagenodeChooseFile
	world
	
pagenodeChooseFile :: Task ()
pagenodeChooseFile =
	pageChooseFile (ActionContinue, pagenodeEditor)

pagenodeEditor :: String String -> Task ()
pagenodeEditor path name =
	pageEditor path name 
	(	(ActionQuit,
			return ())
	,	(Action "import paths",
			pagenodeAskImportPaths)
	)
		
pagenodeAskImportPaths :: String String -> Task ()
pagenodeAskImportPaths path name =
	pageAskImportPaths path name 
	(	(ActionContinue,
			pagenodeEditor)
	,	(ActionCancel,
			pagenodeEditor)
	)
	 