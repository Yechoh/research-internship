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
	pageChooseFile pagenodeEditor

pagenodeEditor :: String String -> Task ()
pagenodeEditor path name =
	pageEditor path name 
	(	(ActionQuit,
			return ())
	,	\p n -> (Action "import paths",
			pagenodeAskImportPaths p n)
	)
		
pagenodeAskImportPaths :: String String -> Task ()
pagenodeAskImportPaths path name =
	pageAskImportPaths path name 
	(	\ p n -> (ActionContinue,
			pagenodeEditor p n)
	,	\ p n -> (ActionCancel,
			pagenodeEditor p n)
	)
	 