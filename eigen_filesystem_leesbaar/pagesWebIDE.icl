module pagesWebIDE

//import iTasks
import pageChooseFile
import pageEditor
import pageAskImportPaths
import qualified Data.Map as DM
import shares
//import settings

Start :: *World -> *World
Start world = startEngine
	(set "C:\\Users\\Martin\\Documents\\clean-bundle-itasks-windows-x86-latest\\research-internship\\eigen_filesystem_leesbaar\\pagesWebIDE.prj" project >>| pagenodeEditor)
	world
	
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
	


