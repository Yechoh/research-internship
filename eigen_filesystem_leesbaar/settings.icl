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
		>>- \mbHome ->  upd (\cs -> {cs & cpmDirectory = if (cs.cpmDirectory == "" && isJust mbHome) (fromJust mbHome) cs.cpmDirectory}) settings 
		>>|				get settings
		>>- \curSet ->	get project
		>>- \recProj -> let isProj 			= recProj.projectPath <> "" && recProj.projectName <> ""
							isCPM  			= curSet.cpmDirectory <> ""
							projectPath 	= recProj.projectPath </> recProj.projectName +++ ".prj"
							applicationPath = curSet.cpmDirectory
						in	(if (not isCPM)  (viewInformation () [] "Where is cpm.exe ? Should be in Clean home directory...")
							(if (not isProj) (viewInformation () [] ("The location of cpm.exe currently set is: " +++ curSet.cpmDirectory )
											 -||-
											 viewInformation () [] "No Project Set...")
					   			(viewInformation () [] "Change Settings..")
							))
							>>*		[ OnAction (Action "Set cpm location")	(always 		(mbCancel findCPM <<@ ApplyLayout frameCompact))
							 		, OnAction (Action "Set Project")		(ifCond isCPM   (mbCancel NewProject <<@ ApplyLayout frameCompact))  
							 		]

	findCPM 
		= 					askUserForFile cpmFile isCPM 
		>>- \(path,file) ->	upd (\curSet -> {curSet & cpmDirectory  = path}) settings

	NewProject  
		= 					askUserForFile "the Clean .icl main file" isCleanIcl 
		>>- \(path,file) -> upd (\myProj -> { myProj 	& projectPath 		= path
														, projectName 		= dropExtension file
														, projectSources 	= [path]
				  							}) project
		>>|					get settings 
		>>- \curSet ->		get project
		>>- \myProj ->		createProject (curSet.cpmDirectory </> cpmFile) myProj.projectPath myProj.projectName
		>>|- setContents (path </> file) 
		
setContents :: String -> Task ()
setContents iclloc
	= 							readLinesFromFile iclloc
	>>- \(Just contenttxt) ->	get contents
	>>- \contentmap ->			set ('DM'.put iclloc contenttxt contentmap) contents
								>>|- return ()
		
mbCancel :: (Task a) -> Task ()
mbCancel ta 
	=
	(		(ta @! ())
	-||- 	(	viewInformation () [] ()
			>>*	[OnAction ActionCancel (always (return ()))]
			)
	)
	>>|- return ()