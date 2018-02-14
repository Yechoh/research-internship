implementation module settings

import System.FilePath, System.Environment, Text.HTML
import directoryBrowsing, createAndRunExec
import shares
import extraTaskCombinators
import qualified Data.Map as DM

setSettings :: Task ()
setSettings = setSettings` <<@ ApplyLayout frameCompact
where
	setSettings`
		=				accWorld (getEnvironmentVariable "CLEAN_HOME")
		>>- \mbHome ->  upd (\cs -> {cs & cleanHome = if (cs.cleanHome == "" && isJust mbHome) ((fromJust mbHome)) cs.cleanHome}) settings
		>>|				get settings
		>>- \curSet ->	get project
		>>- \recProj -> let isProj 			= recProj.projectPath <> "" && recProj.projectName <> ""
							isCPM  			= curSet.cleanHome <> ""
							projectPath 	= recProj.projectPath </> recProj.projectName +++ ".prj"
							applicationPath = curSet.cleanHome
						in	(if (not isCPM)  (viewInformation () [] "Where is the Clean home directory...")
							(if (not isProj) (viewInformation () [] ("The Clean home directory currently set is: " +++ curSet.cleanHome )
											 -||-
											 viewInformation () [] "No Project Set...")
					   			(viewInformation () [] "Change Settings..")
							))
							>>*		[ OnAction (Action "Set Clean home")	(always 		(mbCancel findHome <<@ ApplyLayout frameCompact))
							 		, OnAction (Action "Set Project")		(ifCond isCPM   (mbCancel NewProject <<@ ApplyLayout frameCompact))
							 		]

	findHome
		= 					viewInformation "" [] "where is the Clean home directory?"
		||-					getPwdName
 		>>- \pwd ->			selectFolder pwd
		>>- \(path,file) ->	upd (\curSet -> {curSet & cleanHome  = path}) settings

	NewProject
		= 					askUserForFile "the Clean .icl main file" isCleanIcl
		>>- \(path,file) -> upd (\myProj -> { myProj 	& projectPath 		= path
														, projectName 		= dropExtension file
														, projectSources 	= [path]
				  							}) project
		>>|					get settings
		>>- \curSet ->		get project
		>>- \myProj ->		createProject (curSet.cleanHome </> "bin" </> "cpm") "/" (myProj.projectPath </> myProj.projectName)
		>>|- setContents (path </> file)

setContents :: String -> Task ()
setContents iclloc
	= 							readLinesFromFile iclloc
	>>- \(Just contenttxt) ->	get contents
	>>- \contentmap ->			set (setText iclloc contenttxt contentmap) contents
								>>|- return ()

setText :: String [String] (Map String AceState) -> Map String AceState
setText iclloc contenttxt contentmap = 'DM'.put iclloc {zero & lines=contenttxt} contentmap

mbCancel :: (Task a) -> Task ()
mbCancel ta
	=
	(		(ta @! ())
	-||- 	(	viewInformation () [] ()
			>>*	[OnAction ActionCancel (always (return ()))]
			)
	)
	>>|- return ()
