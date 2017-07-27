module pagesWebIDE

//import iTasks
import pageChooseFile
import pageEditor
import pageAskImportPaths
import qualified Data.Map as DM
import shares
import settings
import pageCreateFile
import errorHandling

Start world = //isAFunctionLineTest//vbselecties//
	startEngine
	//(return () >>* [OnAction (Action "a") (always (return ()))])//
	/*(viewSharedInformation "" [] contents -&&- (forever (enterInformation "" [] 
	>>= (\a. placeText "aaa" 0 a) 
	>>| contentLinesOf "aaa"
	>>= \a. (viewInformation "tada!" [] a) 
	>>| return ())))*/
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
	
pagenodeCreateFile :: Task ()
pagenodeCreateFile = 
	pageCreateFile 
	(	(Action "Create",
			 pagenodeEditor)
	,	(ActionCancel,
			pagenodeEditor)
	)

pagenodeEditor :: Task ()
pagenodeEditor =
	get contents >>- \c.
	if ('DM'.mapSize c == 0)
	pagenodeChooseFile
	(pageEditor
	(	(ActionOpen,
			pagenodeChooseFile)
	,	(Action "Import Paths",
			pagenodeAskImportPaths)
	,	(Action "/File/New",
			pagenodeCreateFile)
	))
		
pagenodeAskImportPaths :: Task ()
pagenodeAskImportPaths=
	pageAskImportPaths
	(	(ActionContinue,
			pagenodeEditor)
	,	(ActionCancel,
			pagenodeEditor)
	)
	


