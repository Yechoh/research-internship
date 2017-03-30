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

pagenodeEditor :: Task ()
pagenodeEditor =
	pageEditor
	(	(ActionQuit,
			return ())
	,	(Action "import paths",
			pagenodeAskImportPaths)
	)
		
pagenodeAskImportPaths :: Task ()
pagenodeAskImportPaths =
	pageAskImportPaths
	(	(ActionContinue,
			pagenodeEditor)
	,	(ActionCancel,
			pagenodeEditor)
	)
	 