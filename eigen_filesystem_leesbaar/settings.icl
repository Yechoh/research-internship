implementation module settings

import System.FilePath, System.Environment, Text.HTML
import ideConstants, ideUtil, directoryBrowsing, createAndRunExec, projectOptions
-
-
settings :: Shared Settings
settings 	= sharedStore "settings" 	{ cpmDirectory	= ""
										} 
environment :: Shared Environment
environment	= sharedStore "evironment" 	{ environmentName = ""
										, environmentLibs = []
										}
project :: Shared Project
project 	= sharedStore "project" 	{ projectName		= ""
										, projectPath 		= ""
										, projectSources	= [] 
										}


setSettings :: Task ()
setSettings = setSettings` <<@ ApplyLayout frameCompact
where 
	setSettings`
		=				accWorld (getEnvironmentVariable "CLEAN_HOME")
		>>- \mbHome ->  upd (\cs -> {cs & cpmDirectory = if (cs.cpmDirectory == "" && isJust mbHome) (fromJust mbHome) cs.cpmDirectory}) settings 
		>>|				get settings
		>>- \curSet ->	get project
		>>-	\project ->	get recentProjects
		>>- \recProj -> let isProj 			= project.projectPath <> "" && project.projectName <> ""
							isCPM  			= curSet.cpmDirectory <> ""
							projectPath 	= project.projectPath </> project.projectName +++ ".prj"
							applicationPath = curSet.cpmDirectory
						in	(if (not isCPM)  (viewInformation () [] "Where is cpm.exe ? Should be in Clean home directory...")
							(if (not isProj) (viewInformation () [] ("The location of cpm.exe currently set is: " +++ curSet.cpmDirectory )
											 -||-
											 viewInformation () [] "No Project Set...")
					   			(viewInformation () [] "Change Settings..")
							))
							>>*		[ OnAction (Action "Set cpm location")	(always 		(mbCancel findCPM <<@ ApplyLayout frameCompact))
							 		, OnAction (Action "Set Project")		(ifCond isCPM   (mbCancel NewProject <<@ ApplyLayout frameCompact))
									, OnAction (Action "Use Recent Project")(ifCond (isCPM && recProj <> []) 
																							(mbCancel useRecentProject <<@ ApplyLayout frameCompact))  
									, OnAction (Action "Project Options")		(ifCond (isCPM && recProj <> []) 
																							(projectOptionsEditor projectPath applicationPath))  
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
		>>|					AddStdEvironment
		>>|					get settings 
		>>- \curSet ->		get project
		>>- \myProj ->		createProject (curSet.cpmDirectory </> cpmFile) myProj.projectPath myProj.projectName
		>>|					addToRecentProjects myProj

	AddStdEvironment :: Task ()
	AddStdEvironment 
		=					get settings
		>>- \curSet ->  	fetchDirectories (curSet.cpmDirectory </> "Libraries") isCleanIclDcl
		>>- \libs ->		upd (\cs -> {cs & environmentName = "MyLibraries"
											, environmentLibs = [libs]
										}) environment @! ()
								
	useRecentProject :: Task ()
	useRecentProject 
		=				get recentProjects
		>>- \recent ->	enterChoice "Select previous project to open..." [ChooseFromGrid (\rec -> rec.projectName)] recent
		>>* 			[ OnAction (Action "Use") (hasValue (\oldProj -> set oldProj project @! () ))
						]	 

